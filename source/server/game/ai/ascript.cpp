#include "ailocal.h"
#include "ai.h"
#include "navigation/aasroutecache.h"
#include "teamplay/objectivebasedteam.h"
#include "bot.h"
#include "../g_as_local.h"
#include "../ascript/addon/addon_any.h"
#include "manager.h"
#include <common/helpers/algorithm.h>

// We have to declare a prototype first (GCC cannot apply attributes to a definition)
#ifndef _MSC_VER
static void ApiError(const char *func, const char *format, ...) __attribute__((format(printf, 2, 3))) __attribute__((noreturn));
#else
__declspec(noreturn) static void ApiError(const char *func, _Printf_format_string_ const char *format, ...);
#endif

static void ApiError(const char *func, const char *format, ...)
{
    char formatBuffer[1024];
    char messageBuffer[2048];
    va_list va;
    va_start(va, format);
    Q_snprintfz(formatBuffer, sizeof(formatBuffer), "%s: %s\n", func, format);
    Q_vsnprintfz(messageBuffer, sizeof(messageBuffer), formatBuffer, va);
    va_end(va);
    G_Error( "%s", messageBuffer );
}

#define API_ERROR(message) ApiError(__FUNCTION__, message)
#define API_ERRORV(message, ...) ApiError(__FUNCTION__, message, __VA_ARGS__)

// Debugging scripts is harder than debugging a native code, use these checks excessively
#define CHECK_ARG(arg) (((arg) == nullptr) ? ( API_ERROR( #arg " is null"), (arg) ) : (arg))

static const asEnumVal_t asNavEntityFlagsEnumVals[] =
{
    ASLIB_ENUM_VAL( AI_NAV_REACH_AT_TOUCH ),
    ASLIB_ENUM_VAL( AI_NAV_REACH_AT_RADIUS ),
    ASLIB_ENUM_VAL( AI_NAV_REACH_ON_EVENT ),
    ASLIB_ENUM_VAL( AI_NAV_REACH_IN_GROUP ),
    ASLIB_ENUM_VAL( AI_NAV_DROPPED ),
    ASLIB_ENUM_VAL( AI_NAV_MOVABLE ),
    ASLIB_ENUM_VAL( AI_NAV_NOTIFY_SCRIPT ),

    ASLIB_ENUM_VAL_NULL
};

static const asEnumVal_t asWeaponAimTypeEnumVals[] =
{
    ASLIB_ENUM_VAL( AI_WEAPON_AIM_TYPE_INSTANT_HIT ),
    ASLIB_ENUM_VAL( AI_WEAPON_AIM_TYPE_PREDICTION ),
    ASLIB_ENUM_VAL( AI_WEAPON_AIM_TYPE_PREDICTION_EXPLOSIVE ),
    ASLIB_ENUM_VAL( AI_WEAPON_AIM_TYPE_DROP ),

    ASLIB_ENUM_VAL_NULL
};

#define DECLARE_ACTION_RECORD_STATUS_ENUM_VAL(value) { "AI_ACTION_RECORD_STATUS_" #value, (int)AiActionRecord::value }

static const asEnumVal_t asActionRecordStatusEnumVals[] =
{
    DECLARE_ACTION_RECORD_STATUS_ENUM_VAL(COMPLETED),
    DECLARE_ACTION_RECORD_STATUS_ENUM_VAL(VALID),
    DECLARE_ACTION_RECORD_STATUS_ENUM_VAL(INVALID),

    ASLIB_ENUM_VAL_NULL
};

const asEnum_t asAIEnums[] =
{
    { "nav_entity_flags_e", asNavEntityFlagsEnumVals },
    { "weapon_aim_type_e", asWeaponAimTypeEnumVals },
    { "action_record_status_e", asActionRecordStatusEnumVals },

    ASLIB_ENUM_VAL_NULL
};

static const asFuncdef_t EMPTY_FUNCDEFS[] = { ASLIB_FUNCDEF_NULL };
static const asBehavior_t EMPTY_BEHAVIORS[] = { ASLIB_BEHAVIOR_NULL };
static const asProperty_t EMPTY_PROPERTIES[] = { ASLIB_PROPERTY_NULL };
static const asMethod_t EMPTY_METHODS[] = { ASLIB_METHOD_NULL };

#define DECLARE_METHOD(type, name, params, nativeFunc) \
    { ASLIB_FUNCTION_DECL(type, name, params), asFUNCTION(nativeFunc), asCALL_CDECL_OBJFIRST }

#define DECLARE_METHOD_PAIR(type, name, params, nativeFunc)   \
    DECLARE_METHOD(type, name, params, nativeFunc),           \
    DECLARE_METHOD(const type, name, params const, nativeFunc)


// We bypass the stock CScriptAny::Retrieve() method and access the value object pointer directly
// to avoid extra performance issues/leaks due to operations on the object ref count performed in the method.
// However this raw object pointer is not all what we need.
// Scripts can pass a pointer to an object of a wrong type in the `any` container leading to an app crash.
// We want to control this and thus implement a custom type checker for a value passed in the `any` container.
// We can use CScriptAny::GetTypeId(), asIScriptEngine::GetObjectTypeById() and so on, but this is a rather slow approach.
// All subtypes of a needed type are known after script loading.
// We store ids of all these types and check an actual value type id against these ids.
// Considering that there are usually few subtypes of a needed type in the script code, this approach is very fast.
class TypeHolderAndChecker
{
    const char *name;
    wsw::StaticVector<int, 24> subtypesIds;
public:
    TypeHolderAndChecker(const char *name_): name(name_) {}

    void Load(asIScriptModule *module)
    {
        asIObjectType *baseType = module->GetObjectTypeByDecl(name);
        if (!baseType)
            API_ERRORV("Can't find %s type in the module", name);

        for (unsigned i = 0, end = module->GetObjectTypeCount(); i < end; ++i)
        {
            asIObjectType *type = module->GetObjectTypeByIndex(i);
            if (!type->DerivesFrom(baseType))
                continue;

            if( subtypesIds.full() ) {
            	API_ERRORV("Too many subtypes for type %s\n", name);
            } else {
            	subtypesIds.push_back(type->GetTypeId());
            }
        }
    }

    void Unload() { subtypesIds.clear(); }

    void *GetValueRef(CScriptAny *anyRef)
    {
        int actualTypeId = anyRef->GetTypeId();
        if (!(actualTypeId & asTYPEID_OBJHANDLE))
            API_ERROR("A value stored in `any` container is not a script handle\n");

        // Clear additional flags, keep only potential registered type part
        int maskedTypeId = actualTypeId;
        maskedTypeId &= ~asTYPEID_OBJHANDLE;
        maskedTypeId &= asTYPEID_MASK_OBJECT | asTYPEID_MASK_SEQNBR;
        if (anyRef->value.valueObj)
        {
			if (wsw::contains(subtypesIds, maskedTypeId))
                return anyRef->value.valueObj;
        }

        const char *actualTypeName = (GAME_AS_ENGINE())->GetObjectTypeById(actualTypeId)->GetName();
        int baseTypeId = (GAME_AS_ENGINE())->GetObjectTypeByDecl(name)->GetTypeId();
        const char *format = "A value of type %s (id=%d) stored in `any` container is not of type %s (id=%d)\n";
        API_ERRORV(format, actualTypeName, actualTypeId, name, baseTypeId);
    }
};

static TypeHolderAndChecker scriptWeightConfigVarTypeHolder("AIScriptWeightConfigVar");
static TypeHolderAndChecker scriptWeightConfigVarGroupTypeHolder("AIScriptWeightConfigVarGroup");

static const asProperty_t asAiScriptWeaponDef_Properties[] =
{
    { ASLIB_PROPERTY_DECL(int, weaponNum), ASLIB_FOFFSET(AiScriptWeaponDef, weaponNum) },
    { ASLIB_PROPERTY_DECL(int, tier), ASLIB_FOFFSET(AiScriptWeaponDef, tier) },
    { ASLIB_PROPERTY_DECL(float, minRange), ASLIB_FOFFSET(AiScriptWeaponDef, minRange) },
    { ASLIB_PROPERTY_DECL(float, maxRange), ASLIB_FOFFSET(AiScriptWeaponDef, maxRange) },
    { ASLIB_PROPERTY_DECL(float, bestRange), ASLIB_FOFFSET(AiScriptWeaponDef, bestRange) },
    { ASLIB_PROPERTY_DECL(float, projectileSpeed), ASLIB_FOFFSET(AiScriptWeaponDef, projectileSpeed) },
    { ASLIB_PROPERTY_DECL(float, splashRadius), ASLIB_FOFFSET(AiScriptWeaponDef, splashRadius) },
    { ASLIB_PROPERTY_DECL(float, maxSelfDamage), ASLIB_FOFFSET(AiScriptWeaponDef, maxSelfDamage) },
    { ASLIB_PROPERTY_DECL(weapon_aim_type_e, aimType), ASLIB_FOFFSET(AiScriptWeaponDef, aimType) },
    { ASLIB_PROPERTY_DECL(bool, isContinuousFire), ASLIB_FOFFSET(AiScriptWeaponDef, isContinuousFire) },

    ASLIB_PROPERTY_NULL
};

static const asClassDescriptor_t asAiScriptWeaponDefClassDescriptor =
{
    "AIScriptWeaponDef",		         /* name */
    asOBJ_VALUE|asOBJ_POD,	             /* object type flags */
    sizeof(AiScriptWeaponDef),	         /* size */
    EMPTY_FUNCDEFS,		                 /* funcdefs */
    EMPTY_BEHAVIORS,                     /* object behaviors */
    EMPTY_METHODS,	                     /* methods */
    asAiScriptWeaponDef_Properties,		 /* properties */

    NULL, NULL					         /* string factory hack */
};

static constexpr auto DEFAULT_MAX_DEFENDERS = 5;
static constexpr auto DEFAULT_MAX_ATTACKERS = 5;

// Unused but mandatory for non-POD AS objects
static void objectAiDefenceSpot_defaultConstructor( void *mem )
{
    new( mem )AiDefenceSpot();
}

static void objectAiDefenceSpot_constructor( void *mem, int id, const edict_t *entity, float radius )
{
    auto *spot = new( mem )AiDefenceSpot;
    spot->id = id;
    spot->entity = entity;
    spot->radius = radius;
    spot->usesAutoAlert = true;
    spot->alertMessage = nullptr;
    spot->minAssignedBots = 1;
    spot->maxAssignedBots = DEFAULT_MAX_DEFENDERS;
    spot->regularEnemyAlertScale = 1.0f;
    spot->carrierEnemyAlertScale = 1.0f;
}

static void objectAiDefenceSpot_destructor( void *mem )
{
    ( ( AiDefenceSpot *) mem )->~AiDefenceSpot();
}

#define DECLARE_CONSTRUCTOR(params, nativeFunc) \
    { asBEHAVE_CONSTRUCT, ASLIB_FUNCTION_DECL(void, f, params), asFUNCTION(nativeFunc), asCALL_CDECL_OBJFIRST }

#define DECLARE_DESTRUCTOR(params, nativeFunc) \
    { asBEHAVE_DESTRUCT, ASLIB_FUNCTION_DECL(void, f, params), asFUNCTION(nativeFunc), asCALL_CDECL_OBJFIRST }

// These script bindings are a mess and must be refactored ASAP.
// Let us limit to fields really used by scripts for testing objective-based gametypes right now.

#define DEFINE_SPOT_ACCESSORS( SpotClass, type, field )                                  \
static type objectAi##SpotClass##_get##field( Ai##SpotClass *spot )                      \
    { return spot->field; }                                                              \
static void objectAi##SpotClass##_set##field( Ai##SpotClass *spot, type value )          \
    { spot->field = value; }

#define DEFINE_DEFENCE_SPOT_ACCESSORS( type, field ) \
    DEFINE_SPOT_ACCESSORS( DefenceSpot, type, field )

DEFINE_DEFENCE_SPOT_ACCESSORS( int, minAssignedBots )
DEFINE_DEFENCE_SPOT_ACCESSORS( int, maxAssignedBots )
DEFINE_DEFENCE_SPOT_ACCESSORS( float, regularEnemyAlertScale )
DEFINE_DEFENCE_SPOT_ACCESSORS( float, carrierEnemyAlertScale )

#define DECLARE_SPOT_ACCESSORS( SpotClass, scriptType, scriptField, nativeField )                           \
DECLARE_METHOD(void, set_##scriptField, (scriptType value), objectAi##SpotClass##_set##nativeField),\
DECLARE_METHOD(scriptType, get_##scriptField, (), objectAi##SpotClass##_get##nativeField)

#define DECLARE_DEFENCE_SPOT_ACCESSORS( scriptType, scriptField, nativeField ) \
    DECLARE_SPOT_ACCESSORS( DefenceSpot, scriptType, scriptField, nativeField )

static void objectAiDefenceSpot_setAlertMessage( AiDefenceSpot *spot, asstring_t *message )
{
    if( !message || !message->buffer ) {
        spot->alertMessage = nullptr;
        return;
    }

    spot->alertMessage = G_RegisterLevelString( message->buffer );
}

static const asBehavior_t asAiDefenceSpot_ObjectBehaviors[] =
{
	DECLARE_CONSTRUCTOR((), objectAiDefenceSpot_defaultConstructor),
    DECLARE_CONSTRUCTOR((int id, const Entity @entity, float radius), objectAiDefenceSpot_constructor),
    DECLARE_DESTRUCTOR((), objectAiDefenceSpot_destructor),

    ASLIB_BEHAVIOR_NULL
};

static const asMethod_t asAiDefenceSpot_Methods[] = {
    DECLARE_DEFENCE_SPOT_ACCESSORS( int, minDefenders, minAssignedBots ),
    DECLARE_DEFENCE_SPOT_ACCESSORS( int, maxDefenders, maxAssignedBots ),
    DECLARE_DEFENCE_SPOT_ACCESSORS( int, regularEnemyAlertScale, regularEnemyAlertScale ),
    DECLARE_DEFENCE_SPOT_ACCESSORS( int, carrierEnemyAlertScale, carrierEnemyAlertScale ),

    DECLARE_METHOD( void, set_alertMessage, (const String &), objectAiDefenceSpot_setAlertMessage ),

    ASLIB_METHOD_NULL
};

static const asClassDescriptor_t asAiDefenceSpotClassDescriptor =
{
    "AIDefenceSpot",
    asOBJ_VALUE,
    sizeof(AiDefenceSpot),
    EMPTY_FUNCDEFS,
    asAiDefenceSpot_ObjectBehaviors,
    asAiDefenceSpot_Methods,
    EMPTY_PROPERTIES,

    NULL, NULL
};

static void objectAiOffenseSpot_defaultConstructor( void *mem )
{
    new( mem )AiOffenseSpot;
}

static void objectAiOffenseSpot_constructor( void *mem, int id, const edict_t *entity )
{
    auto *spot = new( mem )AiOffenseSpot;
    spot->id = id;
    spot->entity = entity;
    spot->minAssignedBots = 1;
    spot->maxAssignedBots = DEFAULT_MAX_ATTACKERS;
}

static void objectAiOffenseSpot_destructor( void *mem )
{
    ( ( AiOffenseSpot * ) mem )->~AiOffenseSpot();
}

#define DEFINE_OFFENSE_SPOT_ACCESSORS( type, field ) \
    DEFINE_SPOT_ACCESSORS( OffenseSpot, type, field )

#define DECLARE_OFFENSE_SPOT_ACCESSORS( scriptType, scriptField, nativeField ) \
    DECLARE_SPOT_ACCESSORS( OffenseSpot, scriptType, scriptField, nativeField )

DEFINE_OFFENSE_SPOT_ACCESSORS( int, minAssignedBots )
DEFINE_OFFENSE_SPOT_ACCESSORS( int, maxAssignedBots )

static const asBehavior_t asAiOffenseSpot_ObjectBehaviors[] =
{
	DECLARE_CONSTRUCTOR((), objectAiOffenseSpot_defaultConstructor),
    DECLARE_CONSTRUCTOR((int id, const Entity @entity), objectAiOffenseSpot_constructor),
    DECLARE_DESTRUCTOR((), objectAiOffenseSpot_destructor),

    ASLIB_BEHAVIOR_NULL
};

static const asMethod_t asAiOffenseSpot_ObjectMethods[] =
{
    DECLARE_OFFENSE_SPOT_ACCESSORS( int, minAttackers, minAssignedBots ),
    DECLARE_OFFENSE_SPOT_ACCESSORS( int, maxAttackers, maxAssignedBots ),

    ASLIB_METHOD_NULL
};

static const asClassDescriptor_t asAiOffenseSpotClassDescriptor =
{
    "AIOffenseSpot",
    asOBJ_VALUE,
    sizeof(AiOffenseSpot),
    EMPTY_FUNCDEFS,
    asAiOffenseSpot_ObjectBehaviors,
    asAiOffenseSpot_ObjectMethods,
    EMPTY_PROPERTIES,

    NULL, NULL
};

static void objectCampingSpot_constructor1( AiCampingSpot *obj, const asvec3_t *origin, float radius, float alertness)
{
    new(CHECK_ARG(obj))AiCampingSpot(CHECK_ARG(origin)->v, radius, alertness);
}

static void objectCampingSpot_constructor2( AiCampingSpot *obj, const asvec3_t *origin, const asvec3_t *lookAtPoint, float radius, float alertness)
{
    new(CHECK_ARG(obj))AiCampingSpot(CHECK_ARG(origin)->v, CHECK_ARG(lookAtPoint)->v, radius, alertness);
}

static const asBehavior_t asAiCampingSpot_ObjectBehaviors[] =
{
    DECLARE_CONSTRUCTOR((const Vec3 &in radius, float radius, float alertness = 0.75f), objectCampingSpot_constructor1),
    DECLARE_CONSTRUCTOR((const Vec3 &in radius, const Vec3 &in lookAtPoint, float radius, float alertness = 0.75f), objectCampingSpot_constructor2),

    ASLIB_BEHAVIOR_NULL
};

static asvec3_t objectCampingSpot_get_origin(const AiCampingSpot *obj)
{
    asvec3_t result;
    obj->Origin().CopyTo(result.v);
    return result;
}

static asvec3_t objectCampingSpot_get_lookAtPoint(const AiCampingSpot *obj)
{
    asvec3_t result;
    obj->LookAtPoint().CopyTo(result.v);
    return result;
}

static float objectCampingSpot_get_radius(const AiCampingSpot *obj) { return obj->Radius(); }
static float objectCampingSpot_get_alertness(const AiCampingSpot *obj) { return obj->Alertness(); }
static float objectCampingSpot_get_hasLookAtPoint(const AiCampingSpot *obj) { return obj->hasLookAtPoint; }

static const asMethod_t asAiCampingSpot_ObjectMethods[] =
{
    DECLARE_METHOD(Vec3, get_origin, () const, objectCampingSpot_get_origin),
    DECLARE_METHOD(Vec3, get_lookAtPoint, () const, objectCampingSpot_get_lookAtPoint),
    DECLARE_METHOD(float, get_radius, () const, objectCampingSpot_get_radius),
    DECLARE_METHOD(float, get_alertness, () const, objectCampingSpot_get_alertness),
    DECLARE_METHOD(bool, get_hasLookAtPoint, () const, objectCampingSpot_get_hasLookAtPoint),

    ASLIB_METHOD_NULL
};

static const asClassDescriptor_t asAiCampingSpotClassDescriptor =
{
    "AICampingSpot",
    asOBJ_VALUE|asOBJ_POD,
    sizeof(AiCampingSpot),
    EMPTY_FUNCDEFS,
    asAiCampingSpot_ObjectBehaviors,
    asAiCampingSpot_ObjectMethods,
    EMPTY_PROPERTIES,

    NULL, NULL
};

static void objectAiPendingLookAtPoint_constructor( AiPendingLookAtPoint *obj, const asvec3_t *origin, float turnSpeedMultiplier )
{
    new (CHECK_ARG(obj))AiPendingLookAtPoint(origin->v, turnSpeedMultiplier);
}

static const asBehavior_t asAiPendingLookAtPoint_ObjectBehaviors[] =
{
    DECLARE_CONSTRUCTOR((const Vec3 &in origin, float turnSpeedMultiplier), objectAiPendingLookAtPoint_constructor),

    ASLIB_BEHAVIOR_NULL
};

static asvec3_t objectPendingLookAtPoint_get_origin(const AiPendingLookAtPoint *obj)
{
    asvec3_t result;
    obj->Origin().CopyTo(result.v);
    return result;
}

static float objectPendingLookAtPoint_get_turnSpeedMultiplier(const AiPendingLookAtPoint *obj)
{
    return obj->TurnSpeedMultiplier();
}

static const asMethod_t asAiPendingLookAtPoint_ObjectMethods[] =
{
    DECLARE_METHOD(Vec3, origin, () const, objectPendingLookAtPoint_get_origin),
    DECLARE_METHOD(float, turnSpeedMultiplier, () const, objectPendingLookAtPoint_get_turnSpeedMultiplier),

    ASLIB_METHOD_NULL
};

static const asClassDescriptor_t asAiPendingLookAtPointClassDescriptor =
{
    "AIPendingLookAtPoint",
    asOBJ_VALUE|asOBJ_POD,
    sizeof(AiPendingLookAtPoint),
    EMPTY_FUNCDEFS,
    asAiPendingLookAtPoint_ObjectBehaviors,
    asAiPendingLookAtPoint_ObjectMethods,
    EMPTY_PROPERTIES,

    NULL, NULL
};

/*
static bool objectSelectedNavEntity_isValid(const SelectedNavEntity *obj) { return obj->IsValid(); }
static bool objectSelectedNavEntity_isEmpty(const SelectedNavEntity *obj) { return obj->IsEmpty(); }

static bool objectSelectedNavEntity_isBasedOnEntity(const SelectedNavEntity *obj, const edict_t *ent )
{
    return CHECK_ARG(obj)->GetNavEntity()->IsBasedOnEntity(ent);
}
static unsigned objectSelectedNavEntity_get_spawnTime(const SelectedNavEntity *obj)
{
    return CHECK_ARG(obj)->GetNavEntity()->SpawnTime();
}
static unsigned objectSelectedNavEntity_get_timeout(const SelectedNavEntity *obj)
{
    return CHECK_ARG(obj)->GetNavEntity()->Timeout();
}
static unsigned objectSelectedNavEntity_get_maxWaitDuration(const SelectedNavEntity *obj)
{
    return CHECK_ARG(obj)->GetNavEntity()->MaxWaitDuration();
}
static float objectSelectedNavEntity_get_cost(const SelectedNavEntity *obj)
{
    return CHECK_ARG(obj)->GetCost();
}
static float objectSelectedNavEntity_get_pickupGoalWeight(const SelectedNavEntity *obj)
{
    return CHECK_ARG(obj)->PickupGoalWeight();
}

// There are currently no reasons to expose all methods of SelectedNavEntity or underlying NavEntity.
// Only methods that are required for a basic GOAP support from the script side are provided.
static const asMethod_t asAiSelectedNavEntity_ObjectMethods[] =
{
    DECLARE_METHOD(bool, isValid, () const, objectSelectedNavEntity_isValid),
    DECLARE_METHOD(bool, isEmpty, () const, objectSelectedNavEntity_isEmpty),
    DECLARE_METHOD(bool, isBasedOnEntity, (const Entity @ent) const, objectSelectedNavEntity_isBasedOnEntity),
    DECLARE_METHOD(uint, get_spawnTime, () const, objectSelectedNavEntity_get_spawnTime),
    DECLARE_METHOD(uint, get_timeout, () const, objectSelectedNavEntity_get_timeout),
    DECLARE_METHOD(uint, get_maxWaitDuration, () const, objectSelectedNavEntity_get_maxWaitDuration),
    DECLARE_METHOD(uint, get_cost, () const, objectSelectedNavEntity_get_cost),
    DECLARE_METHOD(uint, get_pickupGoalWeight, () const, objectSelectedNavEntity_get_pickupGoalWeight),

    ASLIB_METHOD_NULL
};

static const asClassDescriptor_t asAiSelectedNavEntityClassDescriptor =
{
    "AISelectedNavEntity",
    asOBJ_REF|asOBJ_NOCOUNT,
    sizeof(SelectedNavEntity),
    EMPTY_FUNCDEFS,
    EMPTY_BEHAVIORS,
    asAiSelectedNavEntity_ObjectMethods,
    EMPTY_PROPERTIES,

    NULL, NULL
};*/

static AiWeightConfigVar *objectWeightConfigVarGroup_get_varsListHead(AiWeightConfigVarGroup *obj)
{
    return CHECK_ARG(obj)->VarsListHead();
}

static AiWeightConfigVarGroup *objectWeightConfigVarGroup_get_groupsListHead(AiWeightConfigVarGroup *obj)
{
    return CHECK_ARG(obj)->GroupsListHead();
}

static AiWeightConfigVarGroup *objectWeightConfigVarGroup_get_next(AiWeightConfigVarGroup *obj)
{
    return CHECK_ARG(obj)->Next();
}

static unsigned objectWeightConfigVarGroup_get_nameHash(const AiWeightConfigVarGroup *obj)
{
    return CHECK_ARG(obj)->NameHash();
}

static const asstring_t *objectWeightConfigVarGroup_get_name(const AiWeightConfigVarGroup *obj)
{
    const char *nameData = CHECK_ARG(obj)->Name();
    return qasStringFactoryBuffer(nameData, (unsigned)strlen(nameData));
}

static AiWeightConfigVar *objectWeightConfigVarGroup_getVarByName(AiWeightConfigVarGroup *group, const asstring_t *name, unsigned nameHash)
{
    const char *nameData = CHECK_ARG(CHECK_ARG(name)->buffer);
    return CHECK_ARG(group)->GetVarByName(nameData, nameHash);
}

static AiWeightConfigVarGroup *objectWeightConfigVarGroup_getGroupByName(AiWeightConfigVarGroup *group, const asstring_t *name, unsigned nameHash)
{
    const char *nameData = CHECK_ARG(CHECK_ARG(name)->buffer);
    return CHECK_ARG(group)->GetGroupByName(nameData, nameHash);
}

static AiWeightConfigVar *objectWeightConfigVarGroup_getVarByPath(AiWeightConfigVarGroup *group, const asstring_t *name)
{
    const char *nameData = CHECK_ARG(CHECK_ARG(name)->buffer);
    return CHECK_ARG(group)->GetVarByPath(nameData);
}

static AiWeightConfigVarGroup *objectWeightConfigVarGroup_getGroupByPath(AiWeightConfigVarGroup *group, const asstring_t *name)
{
    const char *nameData = CHECK_ARG(CHECK_ARG(name)->buffer);
    return CHECK_ARG(group)->GetGroupByPath(nameData);
}

static void objectWeightConfigVarGroup_addScriptVar(AiWeightConfigVarGroup *group, CScriptAny *varObjAnyRef, const asstring_t *name)
{
    void *scriptObject = scriptWeightConfigVarTypeHolder.GetValueRef(CHECK_ARG(varObjAnyRef));
    const char *nameData = CHECK_ARG(CHECK_ARG(name)->buffer);
    CHECK_ARG(group)->AddScriptVar(nameData, scriptObject);
}

static void objectWeightConfigVarGroup_addScriptGroup(AiWeightConfigVarGroup *group, CScriptAny *groupObjAnyRef, const asstring_t *name)
{
    void *scriptObject = scriptWeightConfigVarGroupTypeHolder.GetValueRef(CHECK_ARG(groupObjAnyRef));
    const char *nameData = CHECK_ARG(CHECK_ARG(name)->buffer);
    CHECK_ARG(group)->AddScriptGroup(nameData, scriptObject);
}

static asMethod_t asAiWeightConfigVarGroup_ObjectMethods[] =
{
    DECLARE_METHOD(const String @, get_name, () const, objectWeightConfigVarGroup_get_name),
    DECLARE_METHOD(uint, get_nameHash, () const, objectWeightConfigVarGroup_get_nameHash),

    DECLARE_METHOD_PAIR(AIWeightConfigVarGroup @, get_next, (), objectWeightConfigVarGroup_get_next),

    DECLARE_METHOD_PAIR(AIWeightConfigVar @, get_varsListHead, (), objectWeightConfigVarGroup_get_varsListHead),
    DECLARE_METHOD_PAIR(AIWeightConfigVarGroup @, getGroupsListHead, (), objectWeightConfigVarGroup_get_groupsListHead),

    DECLARE_METHOD_PAIR(AIWeightConfigVar @, getVarByName, (const String &in name, uint nameHash), objectWeightConfigVarGroup_getVarByName),
    DECLARE_METHOD_PAIR(AIWeightConfigVarGroup @, getGroupByName, (const String &in name, uint nameHash), objectWeightConfigVarGroup_getGroupByName),
    DECLARE_METHOD_PAIR(AIWeightConfigVar @, getVarByPath, (const String &in path), objectWeightConfigVarGroup_getVarByPath),
    DECLARE_METHOD_PAIR(AIWeightConfigVarGroup @, getGroupByPath, (const String &in path), objectWeightConfigVarGroup_getGroupByPath),

    DECLARE_METHOD(void, addScriptVar, (any @scriptWeightConfigVar, const String &in name), objectWeightConfigVarGroup_addScriptVar),
    DECLARE_METHOD(void, addScriptGroup, (any @scriptWeightConfigGroup, const String &in name), objectWeightConfigVarGroup_addScriptGroup),

    ASLIB_METHOD_NULL
};

static const asClassDescriptor_t asAiWeightConfigVarGroupClassDescriptor =
{
    "AIWeightConfigVarGroup",
    asOBJ_REF|asOBJ_NOCOUNT,
    8,                                   /* use dummy size, only refs of this object are exposed to scripts */
    EMPTY_FUNCDEFS,
    EMPTY_BEHAVIORS,
    asAiWeightConfigVarGroup_ObjectMethods,
    EMPTY_PROPERTIES,

    NULL, NULL
};

static AiWeightConfigVar *objectWeightConfigVar_get_next(AiWeightConfigVar *obj)
{
    return CHECK_ARG(obj)->Next();
}

static unsigned objectWeightConfigVar_get_nameHash(const AiWeightConfigVar *obj)
{
    return CHECK_ARG(obj)->NameHash();
}

static const asstring_t *objectWeightConfigVar_get_name(const AiWeightConfigVar *obj)
{
    const char *nameData = CHECK_ARG(obj)->Name();
    return qasStringFactoryBuffer(nameData, (unsigned)strlen(nameData));
}

static void objectWeightConfigVar_getValueProps(const AiWeightConfigVar *obj, float *value, float *minValue, float *maxValue, float *defaultValue)
{
    CHECK_ARG(obj)->GetValueProps(CHECK_ARG(value), CHECK_ARG(minValue), CHECK_ARG(maxValue), CHECK_ARG(defaultValue));
}

static void objectWeightConfigVar_setValue(AiWeightConfigVar *obj, float value)
{
    CHECK_ARG(obj)->SetValue(value);
}

static void objectWeightConfigVar_resetToDefaultValues(AiWeightConfigVar *obj)
{
    return CHECK_ARG(obj)->ResetToDefaultValues();
}

static asMethod_t asAiWeightConfigVar_ObjectMethods[] =
{
    DECLARE_METHOD(const String @, get_name, () const, objectWeightConfigVar_get_name),
    DECLARE_METHOD(uint, get_nameHash, () const, objectWeightConfigVar_get_nameHash),

    DECLARE_METHOD_PAIR(AIWeightConfigVar @, get_next, (), objectWeightConfigVar_get_next),

    DECLARE_METHOD(void, getValueProps, (float &out value, float &out minValue, float &out maxValue, float &out defaultValue) const, objectWeightConfigVar_getValueProps),
    DECLARE_METHOD(void, setValue, (float value), objectWeightConfigVar_setValue),
    DECLARE_METHOD(void, resetToDefaultValues, (), objectWeightConfigVar_resetToDefaultValues),

    ASLIB_METHOD_NULL
};

static const asClassDescriptor_t asAiWeightConfigVarClassDescriptor =
{
    "AIWeightConfigVar",
    asOBJ_REF|asOBJ_NOCOUNT,
    8,                           /* use dummy size, only refs of this object are exposed to scripts */
    EMPTY_FUNCDEFS,
    EMPTY_BEHAVIORS,
    asAiWeightConfigVar_ObjectMethods,
    EMPTY_PROPERTIES,

    NULL, NULL
};

#define CHECK_BOT_HANDLE(ai) (!(ai) ? (API_ERROR("The given bot handle is null\n"), (ai)) : (ai))

float objectBot_getEffectiveOffensiveness(const Bot *bot)
{
    return CHECK_BOT_HANDLE(bot)->GetEffectiveOffensiveness();
}

void objectBot_setBaseOffensiveness(Bot *bot, float baseOffensiveness)
{
    CHECK_BOT_HANDLE(bot)->SetBaseOffensiveness(baseOffensiveness);
}

void objectBot_setAttitude(Bot *bot, edict_t *ent, int attitude)
{
    CHECK_BOT_HANDLE(bot)->SetAttitude(CHECK_ARG(ent), attitude);
}

void objectBot_clearOverriddenEntityWeights(Bot *bot)
{
    CHECK_BOT_HANDLE(bot)->ClearOverriddenEntityWeights();
}

void objectBot_overrideEntityWeight(Bot *bot, edict_t *ent, float weight)
{
    CHECK_BOT_HANDLE(bot)->OverrideEntityWeight(CHECK_ARG(ent), weight);
}

int objectBot_get_defenceSpotId(const Bot *bot)
{
    return CHECK_BOT_HANDLE(bot)->DefenceSpotId();
}

int objectBot_get_offenseSpotId(const Bot *bot)
{
    return CHECK_BOT_HANDLE(bot)->OffenseSpotId();
}

void objectBot_setNavTarget(Bot *bot, const asvec3_t *navTargetOrigin, float reachRadius)
{
    if (reachRadius <= 0.0f)
        API_ERROR("The given reach radius is negative\n");

    CHECK_BOT_HANDLE(bot)->SetNavTarget(Vec3(CHECK_ARG(navTargetOrigin)->v), reachRadius);
}

void objectBot_resetNavTarget(Bot *bot)
{
    CHECK_BOT_HANDLE(bot)->ResetNavTarget();
}

void objectBot_setCampingSpot(Bot *bot, const AiCampingSpot *campingSpot)
{
    CHECK_BOT_HANDLE(bot)->SetCampingSpot(*CHECK_ARG(campingSpot));
}

void objectBot_resetCampingSpot(Bot *bot)
{
    CHECK_BOT_HANDLE(bot)->ResetCampingSpot();
}

void objectBot_setPendingLookAtPoint(Bot *bot, const AiPendingLookAtPoint *pendingLookAtPoint, unsigned timeoutPeriod)
{
    CHECK_BOT_HANDLE(bot)->SetPendingLookAtPoint(*CHECK_ARG(pendingLookAtPoint), timeoutPeriod);
}

void objectBot_resetPendingLookAtPoint(Bot *bot)
{
    CHECK_BOT_HANDLE(bot)->ResetPendingLookAtPoint();
}

unsigned objectBot_nextSimilarWorldStateInstanceId(Bot *bot)
{
    return CHECK_BOT_HANDLE(bot)->NextSimilarWorldStateInstanceId();
}

/*
const SelectedNavEntity *objectBot_get_selectedNavEntity(const Bot *bot)
{
    return &CHECK_BOT_HANDLE(bot)->GetSelectedNavEntity();
}*/

const SelectedEnemy *objectBot_get_selectedEnemy(const Bot *bot)
{
    return nullptr;
}

int objectBot_checkTravelTimeMillis(Bot *bot, const asvec3_t *from, const asvec3_t *to, bool allowUnreachable)
{
    return CHECK_BOT_HANDLE(bot)->CheckTravelTimeMillis(Vec3(from->v), Vec3(to->v), allowUnreachable);
}

#define DEFINE_NATIVE_BOT_MISC_TACTICS_ACCESSORS(lowercasePrefix, uppercasePrefix, restOfTheName) \
bool objectBot_get##lowercasePrefix##restOfTheName(const Bot *bot)                         \
{                                                                                                 \
    return CHECK_BOT_HANDLE(bot)->GetMiscTactics().lowercasePrefix##restOfTheName;         \
}                                                                                                 \
void objectBot_set##uppercasePrefix##restOfTheName(Bot *bot, bool value)                   \
{                                                                                                 \
    CHECK_BOT_HANDLE(bot)->GetMiscTactics().lowercasePrefix##restOfTheName = value;        \
}

DEFINE_NATIVE_BOT_MISC_TACTICS_ACCESSORS(will, Will, Advance);
DEFINE_NATIVE_BOT_MISC_TACTICS_ACCESSORS(will, Will, Retreat);
DEFINE_NATIVE_BOT_MISC_TACTICS_ACCESSORS(should, Should, BeSilent);
DEFINE_NATIVE_BOT_MISC_TACTICS_ACCESSORS(should, Should, MoveCarefully);
DEFINE_NATIVE_BOT_MISC_TACTICS_ACCESSORS(should, Should, Attack);
DEFINE_NATIVE_BOT_MISC_TACTICS_ACCESSORS(should, Should, KeepXhairOnEnemy);
DEFINE_NATIVE_BOT_MISC_TACTICS_ACCESSORS(will, Will, AttackMelee);
DEFINE_NATIVE_BOT_MISC_TACTICS_ACCESSORS(should, Should, RushHeadless);

#define DECLARE_SCRIPT_BOT_MISC_TACTICS_ACCESSORS(lowercasePrefix, uppercasePrefix, restOfTheName)                     \
DECLARE_METHOD(bool, get_##lowercasePrefix##restOfTheName, () const, objectBot_get##lowercasePrefix##restOfTheName),   \
DECLARE_METHOD(void, set_##lowercasePrefix##restOfTheName, (bool value), objectBot_set##uppercasePrefix##restOfTheName)

static const asMethod_t asbot_Methods[] =
{
    DECLARE_METHOD(float, getEffectiveOffensiveness, () const, objectBot_getEffectiveOffensiveness),
    DECLARE_METHOD(void, setBaseOffensiveness, (float baseOffensiveness), objectBot_setBaseOffensiveness),

    DECLARE_METHOD(void, setAttitude, (const Entity @ent, int attitude), objectBot_setAttitude),

    DECLARE_METHOD(void, clearOverriddenEntityWeights, (), objectBot_clearOverriddenEntityWeights),
    DECLARE_METHOD(void, overrideEntityWeight, (const Entity @ent, float weight), objectBot_overrideEntityWeight),

    DECLARE_METHOD(int, get_defenceSpotId, () const, objectBot_get_defenceSpotId),
    DECLARE_METHOD(int, get_offenseSpotId, () const, objectBot_get_offenseSpotId),

    DECLARE_METHOD(void, setNavTarget, (const Vec3 &in navTargetOrigin, float reachRadius), objectBot_setNavTarget),
    DECLARE_METHOD(void, resetNavTarget, (), objectBot_resetNavTarget),

    DECLARE_METHOD(void, setCampingSpot, (const AICampingSpot &in campingSpot), objectBot_setCampingSpot),
    DECLARE_METHOD(void, resetCampingSpot, (), objectBot_resetCampingSpot),

    DECLARE_METHOD(void, setPendingLookAtPoint, (const AIPendingLookAtPoint &in lookAtPoint, uint timeoutPeriod), objectBot_setPendingLookAtPoint),
    DECLARE_METHOD(void, resetPendingLookAtPoint, (), objectBot_resetPendingLookAtPoint),

    DECLARE_METHOD(uint, nextSimilarWorldStateInstanceId, (), objectBot_nextSimilarWorldStateInstanceId),

    //DECLARE_METHOD(const AISelectedNavEntity @, get_selectedNavEntity, () const, objectBot_get_selectedNavEntity),
    //DECLARE_METHOD(const AISelectedEnemy @, get_selectedEnemy, () const, objectBot_get_selectedEnemy),

    DECLARE_METHOD(int, checkTravelTimeMillis, (const Vec3 &in from, const Vec3 &in to, bool allowUnreachable), objectBot_checkTravelTimeMillis),

    DECLARE_SCRIPT_BOT_MISC_TACTICS_ACCESSORS(will, Will, Advance),
    DECLARE_SCRIPT_BOT_MISC_TACTICS_ACCESSORS(will, Will, Retreat),
    DECLARE_SCRIPT_BOT_MISC_TACTICS_ACCESSORS(should, Should, BeSilent),
    DECLARE_SCRIPT_BOT_MISC_TACTICS_ACCESSORS(should, Should, MoveCarefully),
    DECLARE_SCRIPT_BOT_MISC_TACTICS_ACCESSORS(should, Should, Attack),
    DECLARE_SCRIPT_BOT_MISC_TACTICS_ACCESSORS(should, Should, KeepXhairOnEnemy),
    DECLARE_SCRIPT_BOT_MISC_TACTICS_ACCESSORS(will, Will, AttackMelee),
    DECLARE_SCRIPT_BOT_MISC_TACTICS_ACCESSORS(should, Should, RushHeadless),

    ASLIB_METHOD_NULL
};

static const asClassDescriptor_t asBotClassDescriptor =
{
    "Bot",						/* name */
    asOBJ_REF|asOBJ_NOCOUNT,	/* object type flags */
    sizeof( Bot ),		/* size */
    EMPTY_FUNCDEFS,				/* funcdefs */
    EMPTY_BEHAVIORS,     		/* object behaviors */
    asbot_Methods,				/* methods */
    EMPTY_PROPERTIES,			/* properties */

    NULL, NULL					/* string factory hack */
};

const asClassDescriptor_t *asAIClassesDescriptors[] =
{
    &asAiScriptWeaponDefClassDescriptor,
    &asAiDefenceSpotClassDescriptor,
    &asAiOffenseSpotClassDescriptor,
    &asAiCampingSpotClassDescriptor,
    &asAiPendingLookAtPointClassDescriptor,
    //&asAiSelectedNavEntityClassDescriptor,
    //&asAiSelectedEnemyClassDescriptor,
    &asAiWeightConfigVarGroupClassDescriptor,
    &asAiWeightConfigVarClassDescriptor,
    &asBotClassDescriptor,

    NULL
};

static inline AiObjectiveBasedTeam *GetObjectiveBasedTeam(const char *caller, int team)
{
    CHECK_ARG(caller);
    // Make sure that AiBaseTeam::GetTeamForNum() will not crash for illegal team
    if (team != TEAM_ALPHA && team != TEAM_BETA)
        API_ERRORV("%s: illegal team %d\n", caller, team);

    // We are not allowed to fail anymore, force switching to objective-based AI team if needed
    AiBaseTeam *baseTeam = AiBaseTeam::GetTeamForNumAndType<AiObjectiveBasedTeam>( team );
    return static_cast<AiObjectiveBasedTeam*>( baseTeam );
}

void asFunc_AddDefenceSpot( int team, const AiDefenceSpot *spot )
{
    GetObjectiveBasedTeam(__FUNCTION__, team)->AddDefenceSpot(*spot);
}

void asFunc_RemoveDefenceSpot( int team, int id )
{
    GetObjectiveBasedTeam(__FUNCTION__, team)->RemoveDefenceSpot(id);
}

void asFunc_DefenceSpotAlert( int team, int id, float alertLevel, unsigned timeoutPeriod )
{
    GetObjectiveBasedTeam(__FUNCTION__, team)->SetDefenceSpotAlert(id, alertLevel, timeoutPeriod);
}

void asFunc_AddOffenseSpot( int team, const AiOffenseSpot *spot )
{
    GetObjectiveBasedTeam(__FUNCTION__, team)->AddOffenseSpot(*spot);
}

void asFunc_RemoveOffenseSpot( int team, int id )
{
    GetObjectiveBasedTeam(__FUNCTION__, team)->RemoveOffenseSpot(id);
}

void asFunc_RemoveAllObjectiveSpots()
{
    if( !GS_TeamBasedGametype( *ggs ) ) {
        return;
    }
    GetObjectiveBasedTeam(__FUNCTION__, TEAM_ALPHA)->RemoveAllObjectiveSpots();
    GetObjectiveBasedTeam(__FUNCTION__, TEAM_BETA)->RemoveAllObjectiveSpots();
}

#define DECLARE_FUNC(signature, nativeFunc) { signature, asFUNCTION(nativeFunc), NULL }

const asglobfuncs_t asAIGlobFuncs[] =
{
    DECLARE_FUNC("void AddNavEntity( Entity @ent, int flags )", AI_AddNavEntity),
    DECLARE_FUNC("void RemoveNavEntity( Entity @ent )", AI_RemoveNavEntity),
    DECLARE_FUNC("void NavEntityReached( Entity @ent )", AI_NavEntityReached),

    DECLARE_FUNC("void AddDefenceSpot(int team, const AIDefenceSpot &in spot )", asFunc_AddDefenceSpot),
    DECLARE_FUNC("void AddOffenseSpot(int team, const AIOffenseSpot &in spot )", asFunc_AddOffenseSpot),
    DECLARE_FUNC("void RemoveDefenceSpot(int team, int id)", asFunc_RemoveDefenceSpot),
    DECLARE_FUNC("void RemoveOffenseSpot(int team, int id)", asFunc_RemoveOffenseSpot),

	DECLARE_FUNC("void RemoveAllObjectiveSpots()", asFunc_RemoveAllObjectiveSpots),

    DECLARE_FUNC("void DefenceSpotAlert(int team, int id, float level, uint timeoutPeriod)", asFunc_DefenceSpotAlert),

    { NULL }
};

// Forward declarations of ASFunctionsRegistry methods result types
template <typename R> struct ASFunction0;
template <typename R, typename T1> struct ASFunction1;
template <typename R, typename T1, typename T2> struct ASFunction2;
template <typename R, typename T1, typename T2, typename T3> struct ASFunction3;
template <typename R, typename T1, typename T2, typename T3, typename T4, typename T5> struct ASFunction5;

class ASFunctionsRegistry
{
    friend struct ASUntypedFunction;

    wsw::StaticVector<struct ASUntypedFunction *, 64> functions;

    inline void Register(struct ASUntypedFunction &function)
    {
        functions.push_back(&function);
    }
public:
    void Load(asIScriptModule *module);
    void Unload();

    template <typename R>
    inline ASFunction0<R> Function0(const char *name, R defaultResult);

    template <typename R, typename T1>
    inline ASFunction1<R, T1> Function1(const char *name, R defaultResult);

    template <typename R, typename T1, typename T2>
    inline ASFunction2<R, T1, T2> Function2(const char *name, R defaultResult);

    template <typename R, typename T1, typename T2, typename T3>
    inline ASFunction3<R, T1, T2, T3> Function3(const char *name, R defaultResult);

    template <typename R, typename T1, typename T2, typename T3, typename T4, typename T5>
    inline ASFunction5<R, T1, T2, T3, T4, T5> Function5(const char *name, R defaultResult);
};

// This class is a common non-template supertype for concrete types.
// We can't store templates (and pointers to these templates) with different parameters
// in a single array in ASFunctionsRegistry due to C++ template invariance.
struct ASUntypedFunction
{
private:
    const char *const decl;
    asIScriptFunction *funcPtr;
protected:
    inline asIScriptContext *PrepareContext()
    {
        if (!funcPtr)
            return nullptr;

        asIScriptContext *ctx = qasAcquireContext(GAME_AS_ENGINE());
        int error = ctx->Prepare(funcPtr);
        if (error < 0)
            return nullptr;

        return ctx;
    }

    inline asIScriptContext *CallForContext(asIScriptContext *preparedContext)
    {
        int error = preparedContext->Execute();
        // Put likely case first
        if (!G_ExecutionErrorReport(error))
            return preparedContext;

        GT_asShutdownScript();
        return nullptr;
    }
public:
    inline ASUntypedFunction(const char *decl_, ASFunctionsRegistry &registry): decl(decl_), funcPtr(nullptr)
    {
        registry.Register(*this);
    }

    void Load(asIScriptModule *module)
    {
        funcPtr = module->GetFunctionByDecl(decl);
        if (!funcPtr)
        {
            if (developer->integer || sv_cheats->integer)
                G_Printf("* The function '%s' was not present in the script.\n", decl);
        }
    }

    inline void Unload() { funcPtr = nullptr; }

    inline bool IsLoaded() const { return funcPtr != nullptr; }
};

void ASFunctionsRegistry::Load(asIScriptModule *module)
{
    for (auto *func: functions)
        func->Load(module);
}

void ASFunctionsRegistry::Unload()
{
    for (auto *func: functions)
        func->Unload();
}

struct Void
{
    static Void VALUE;
};

Void Void::VALUE;

template <typename R> struct ResultGetter
{
    static inline R Get(asIScriptContext *ctx) = delete;
};

template <typename R> struct ResultGetter<const R&>
{
    static inline const R &Get(asIScriptContext *ctx) { return *((R *)ctx->GetReturnObject()); }
};

template <typename R> struct ResultGetter<R*>
{
    static inline R *Get(asIScriptContext *ctx) { return (R *)ctx->GetReturnObject(); }
};

template <typename R> struct ResultGetter<const R*>
{
    static inline const R *Get(asIScriptContext *ctx) { return (R *)ctx->GetReturnObject(); }
};

template <> struct ResultGetter<bool>
{
    static inline bool Get(asIScriptContext *ctx) { return ctx->GetReturnByte() != 0; }
};

template <> struct ResultGetter<int>
{
    static inline int Get(asIScriptContext *ctx) { return ctx->GetReturnDWord(); }
};

template <> struct ResultGetter<unsigned>
{
    static inline unsigned Get(asIScriptContext *ctx) { return ctx->GetReturnDWord(); }
};

template<> struct ResultGetter<float>
{
    static inline float Get(asIScriptContext *ctx) { return ctx->GetReturnFloat(); }
};

template<> struct ResultGetter<Void>
{
    static inline Void Get(asIScriptContext *ctx) { return Void::VALUE; }
};

template<typename R>
struct ASTypedResultFunction: public ASUntypedFunction
{
    R defaultResult;

    ASTypedResultFunction(const char *decl_, R defaultResult_, ASFunctionsRegistry &registry)
        : ASUntypedFunction(decl_, registry), defaultResult(defaultResult_) {}

protected:
    inline R CallForResult(asIScriptContext *preparedContext)
    {
        if (auto ctx = CallForContext(preparedContext))
            return ResultGetter<R>::Get(ctx);
        return defaultResult;
    }
};

template <typename Arg> struct ArgSetter
{
    static inline void Set(asIScriptContext *ctx, unsigned argNum, Arg arg) = delete;
};

template <typename Arg> struct ArgSetter<const Arg &>
{
    static inline void Set(asIScriptContext *ctx, unsigned argNum, const Arg &arg)
    {
        ctx->SetArgObject(argNum, const_cast<Arg *>(&arg));
    }
};

template <typename Arg> struct ArgSetter<const Arg *>
{
    static inline void Set(asIScriptContext *ctx, unsigned argNum, const Arg *arg)
    {
        ctx->SetArgObject(argNum, const_cast<Arg *>(arg));
    }
};

template <typename Arg> struct ArgSetter<Arg *>
{
    static inline void Set(asIScriptContext *ctx, unsigned argNum, Arg *arg)
    {
        ctx->SetArgObject(argNum, arg);
    }
};

// Hack for this primitive type, set a raw address, do not try to modify reference count of a non-existing object
template<> struct ArgSetter<float *>
{
    static inline void Set(asIScriptContext *ctx, unsigned argNum, float *arg)
    {
        ctx->SetArgAddress(argNum, arg);
    }
};

template<> struct ArgSetter<bool>
{
    static inline void Set(asIScriptContext *ctx, unsigned argNum, bool arg)
    {
        ctx->SetArgByte(argNum, arg ? 1 : 0);
    }
};

template<> struct ArgSetter<int>
{
    static inline void Set(asIScriptContext *ctx, unsigned argNum, int arg)
    {
        ctx->SetArgDWord(argNum, arg);
    }
};

template<> struct ArgSetter<float>
{
    static inline void Set(asIScriptContext *ctx, unsigned argNum, int arg)
    {
        ctx->SetArgFloat(argNum, arg);
    }
};

template<typename R>
struct ASFunction0: public ASTypedResultFunction<R>
{
    ASFunction0(const char *decl_, R defaultResult_, ASFunctionsRegistry &registry)
        : ASTypedResultFunction<R>(decl_, defaultResult_, registry) {}

    R operator()()
    {
        if (auto preparedContext = this->PrepareContext())
        {
            return this->CallForResult(preparedContext);
        }
        return this->defaultResult;
    }
};

template<typename R, typename T1>
struct ASFunction1: public ASTypedResultFunction<R>
{
    ASFunction1(const char *decl_, R defaultResult_, ASFunctionsRegistry &registry)
        : ASTypedResultFunction<R>(decl_, defaultResult_, registry) {}

    R operator()(T1 arg1)
    {
        if (auto preparedContext = this->PrepareContext())
        {
            ArgSetter<T1>::Set(preparedContext, 0, arg1);
            return this->CallForResult(preparedContext);
        }
        return this->defaultResult;
    }
};

template<typename R, typename T1, typename T2>
struct ASFunction2: public ASTypedResultFunction<R>
{
    ASFunction2(const char *decl_, R defaultResult_, ASFunctionsRegistry &registry)
        : ASTypedResultFunction<R>(decl_, defaultResult_, registry) {}

    R operator()(T1 arg1, T2 arg2)
    {
        if (auto preparedContext = this->PrepareContext())
        {
            ArgSetter<T1>::Set(preparedContext, 0, arg1);
            ArgSetter<T2>::Set(preparedContext, 1, arg2);
            return this->CallForResult(preparedContext);
        }
        return this->defaultResult;
    }
};

template<typename R, typename T1, typename T2, typename T3>
struct ASFunction3: public ASTypedResultFunction<R>
{
    ASFunction3(const char *decl_, R defaultResult_, ASFunctionsRegistry &registry)
        : ASTypedResultFunction<R>(decl_, defaultResult_, registry) {}

    R operator()(T1 arg1, T2 arg2, T3 arg3)
    {
        if (auto preparedContext = this->PrepareContext())
        {
            ArgSetter<T1>::Set(preparedContext, 0, arg1);
            ArgSetter<T2>::Set(preparedContext, 1, arg2);
            ArgSetter<T3>::Set(preparedContext, 2, arg3);
            return this->CallForResult(preparedContext);
        }
        return this->defaultResult;
    }
};

template<typename R, typename T1, typename T2, typename T3, typename T4, typename T5>
struct ASFunction5: public ASTypedResultFunction<R>
{
    ASFunction5(const char *decl_, R defaultResult_, ASFunctionsRegistry &registry)
        : ASTypedResultFunction<R>(decl_, defaultResult_, registry) {}

    R operator()(T1 arg1, T2 arg2, T3 arg3, T4 arg4, T5 arg5)
    {
        if (auto preparedContext = this->PrepareContext())
        {
            ArgSetter<T1>::Set(preparedContext, 0, arg1);
            ArgSetter<T2>::Set(preparedContext, 1, arg2);
            ArgSetter<T3>::Set(preparedContext, 2, arg3);
            ArgSetter<T4>::Set(preparedContext, 3, arg4);
            ArgSetter<T5>::Set(preparedContext, 4, arg5);
            return this->CallForResult(preparedContext);
        }
        return this->defaultResult;
    }
};

template <typename R>
ASFunction0<R> ASFunctionsRegistry::Function0(const char *decl, R defaultResult)
{
    return ASFunction0<R>(decl, defaultResult, *this);
}

template <typename R, typename T1>
ASFunction1<R, T1> ASFunctionsRegistry::Function1(const char *decl, R defaultResult)
{
    return ASFunction1<R, T1>(decl, defaultResult, *this);
};

template <typename R, typename T1, typename T2>
ASFunction2<R, T1, T2> ASFunctionsRegistry::Function2(const char *decl, R defaultResult)
{
    return ASFunction2<R, T1, T2>(decl, defaultResult, *this);
};

template <typename R, typename T1, typename T2, typename T3>
ASFunction3<R, T1, T2, T3> ASFunctionsRegistry::Function3(const char *decl, R defaultResult)
{
    return ASFunction3<R, T1, T2, T3>(decl, defaultResult, *this);
};

template <typename R, typename T1, typename T2, typename T3, typename T4, typename T5>
ASFunction5<R, T1, T2, T3, T4, T5> ASFunctionsRegistry::Function5(const char *decl, R defaultResult)
{
    return ASFunction5<R, T1, T2, T3, T4, T5>(decl, defaultResult, *this);
};

static ASFunctionsRegistry gtAIFunctionsRegistry;

void AI_InitGametypeScript(asIScriptModule *module)
{
    gtAIFunctionsRegistry.Load(module);

    scriptWeightConfigVarTypeHolder.Load(module);
    scriptWeightConfigVarGroupTypeHolder.Load(module);
}

void AI_ResetGametypeScript()
{
    gtAIFunctionsRegistry.Unload();

    scriptWeightConfigVarTypeHolder.Unload();
    scriptWeightConfigVarGroupTypeHolder.Unload();
}

static auto registerScriptWeightConfigFunc =
    gtAIFunctionsRegistry.Function2<Void, const AiWeightConfig *, const edict_t *>(
        "void GT_RegisterScriptWeightConfig( const AIWeightConfigVarGroup @nativeGroup, const Entity @owner )",
        Void::VALUE);

void GT_asRegisterScriptWeightConfig(class AiWeightConfig *weightConfig, const edict_t *configOwner)
{
    registerScriptWeightConfigFunc(weightConfig, configOwner);
}

static auto releaseScriptWeightConfigFunc =
    gtAIFunctionsRegistry.Function2<Void, const AiWeightConfig *, const edict_t *>(
        "void GT_ReleaseScriptWeightConfig( const AIWeightConfigVarGroup @nativeGroup, const Entity @owner )",
        Void::VALUE);

void GT_asReleaseScriptWeightConfig(class AiWeightConfig *weightConfig, const edict_t *configOwner)
{
    releaseScriptWeightConfigFunc(weightConfig, configOwner);
}

static auto getScriptWeightConfigVarValuePropsFunc =
    gtAIFunctionsRegistry.Function5<Void, const void *, float *, float *, float *, float *>(
        "void GENERIC_GetScriptWeightConfigVarValueProps( const AIScriptWeightConfigVar @var, "
        "float &out value, float &out minValue, float &out maxValue, float &defaultValue )",
        Void::VALUE);

void GENERIC_asGetScriptWeightConfigVarValueProps(const void *scriptObject, float *value, float *minValue, float *maxValue, float *defaultValue)
{
    getScriptWeightConfigVarValuePropsFunc(scriptObject, value, minValue, maxValue, defaultValue);
}

static auto setScriptWeightConfigVarValueFunc =
    gtAIFunctionsRegistry.Function2<Void, void *, float>(
        "void GENERIC_SetScriptWeightConfigVarValueProps( AIScriptWeightConfigVar @var, float value )", Void::VALUE);

void GENERIC_asSetScriptWeightConfigVarValue(void *scriptObject, float value)
{
    setScriptWeightConfigVarValueFunc(scriptObject, value);
}

static auto getScriptBotEvolutionManagerFunc =
    gtAIFunctionsRegistry.Function0<void *>("AIScriptEvolutionManager @GT_GetScriptBotEvolutionManager()", nullptr);

void *GT_asGetScriptBotEvolutionManager()
{
    return getScriptBotEvolutionManagerFunc();
}

static auto setupConnectingBotWeightsFunc =
    gtAIFunctionsRegistry.Function2<Void, void *, edict_t *>(
        "GENERIC_SetupConnectingBotWeights( AIScriptEvolutionManager @manager, Entity @ent )", Void::VALUE);

void GENERIC_asSetupConnectingBotWeights(void *scriptEvolutionManager, edict_t *ent)
{
    setupConnectingBotWeightsFunc(scriptEvolutionManager, ent);
}

static auto setupRespawningBotWeightsFunc =
    gtAIFunctionsRegistry.Function2<Void, void *, edict_t *>(
        "GENERIC_SetupRespawningBotWeights( AIScriptEvolutionManager @manager, Entity @ent )", Void::VALUE);

void GENERIC_asSetupRespawningBotWeights(void *scriptEvolutionManager, edict_t *ent)
{
    setupRespawningBotWeightsFunc(scriptEvolutionManager, ent);
}

static auto saveEvolutionResultsFunc =
    gtAIFunctionsRegistry.Function1<Void, void *>(
        "GENERIC_SaveEvolutionResults( AIScriptEvolutionManager @manager )", Void::VALUE);

void GENERIC_asSaveEvolutionResults(void *scriptEvolutionManager)
{
    saveEvolutionResultsFunc(scriptEvolutionManager);
}

static auto botWouldDropHealthFunc =
    gtAIFunctionsRegistry.Function1<bool, const Client*>("bool GT_BotWouldDropHealth( const Client @client )", false);

bool GT_asBotWouldDropHealth(const Client *client)
{
    return botWouldDropHealthFunc(client);
}

static auto botDropHealthFunc =
    gtAIFunctionsRegistry.Function1<Void, Client*>("void GT_BotDropHealth( Client @client )", Void::VALUE);

void GT_asBotDropHealth( Client *client )
{
    botDropHealthFunc(client);
}

static auto botWouldDropArmorFunc =
    gtAIFunctionsRegistry.Function1<bool, const Client*>("bool GT_BotWouldDropArmor( const Client @client )", false);

bool GT_asBotWouldDropArmor( const Client *client )
{
    return botWouldDropArmorFunc(client);
}

static auto botDropArmorFunc =
    gtAIFunctionsRegistry.Function1<Void, Client*>("void GT_BotDropArmor( Client @client )", Void::VALUE);

void GT_asBotDropArmor( Client *client )
{
    botDropArmorFunc(client);
}

static auto botTouchedGoalFunc =
    gtAIFunctionsRegistry.Function2<Void, const Bot *, const edict_t *>(
        "void GT_BotTouchedGoal( const Bot @bot, const Entity @goalEnt )", Void::VALUE);

void GT_asBotTouchedGoal(const Bot *bot, const edict_t *goalEnt)
{
    botTouchedGoalFunc(bot, goalEnt);
}

static auto botReachedGoalRadiusFunc =
    gtAIFunctionsRegistry.Function2<Void, const Bot *, const edict_t *>(
        "void GT_BotReachedGoalRadius( const Bot @bot, const Entity @goalEnt )", Void::VALUE);

void GT_asBotReachedGoalRadius(const Bot *bot, const edict_t *goalEnt)
{
    botReachedGoalRadiusFunc(bot, goalEnt);
}

static auto playerOffensiveAbilitiesRatingFunc =
    gtAIFunctionsRegistry.Function1<float, const Client*>(
        "float GT_PlayerOffensiveAbilitiesRating( const Client @client )", 0.5f);

float GT_asPlayerOffensiveAbilitiesRating(const Client *client)
{
    return playerOffensiveAbilitiesRatingFunc(client);
}

static auto playerDefenciveAbilitiesRatingFunc =
    gtAIFunctionsRegistry.Function1<float, const Client*>(
        "float GT_PlayerDefenciveAbilitiesRating( const Client @client )", 0.5f);

float GT_asPlayerDefenciveAbilitiesRating(const Client *client)
{
    return playerDefenciveAbilitiesRatingFunc(client);
}

static auto getScriptWeaponsNumFunc =
    gtAIFunctionsRegistry.Function1<int, const Client*>(
        "int GT_GetScriptWeaponsNum( const Client @client )", 0);

int GT_asGetScriptWeaponsNum(const Client *client)
{
    return getScriptWeaponsNumFunc(client);
}

static auto getScriptWeaponDefFunc =
    gtAIFunctionsRegistry.Function3<bool, const Client*, int, AiScriptWeaponDef *>(
        "bool GT_GetScriptWeaponDef( const Client @client, int weaponNum, AIScriptWeaponDef &out weaponDef )", false);

bool GT_asGetScriptWeaponDef(const Client *client, int weaponNum, AiScriptWeaponDef *weaponDef)
{
    return getScriptWeaponDefFunc(client, weaponNum, weaponDef);
}

static auto getScriptWeaponCooldownFunc =
    gtAIFunctionsRegistry.Function2<int, const Client*, int>(
        "int GT_GetScriptWeaponCooldown( const Client @client, int weaponNum )", INT_MAX);

int GT_asGetScriptWeaponCooldown(const Client *client, int weaponNum)
{
    return getScriptWeaponCooldownFunc(client, weaponNum);
}

static auto selectScriptWeaponFunc =
    gtAIFunctionsRegistry.Function2<bool, Client*, int>(
        "bool GT_SelectScriptWeapon( Client @client, int weaponNum )", false);

bool GT_asSelectScriptWeapon(Client *client, int weaponNum)
{
    return selectScriptWeaponFunc(client, weaponNum);
}

static auto fireScriptWeaponFunc =
    gtAIFunctionsRegistry.Function2<bool, Client*, int>(
        "bool GT_FireScriptWeapon( Client @client, int weaponNum )", false);

bool GT_asFireScriptWeapon(Client *client, int weaponNum)
{
    return fireScriptWeaponFunc(client, weaponNum);
}
