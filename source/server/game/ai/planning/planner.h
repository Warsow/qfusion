#ifndef WSW_bf007929_030f_4a9e_aa7c_4cec0536f531_H
#define WSW_bf007929_030f_4a9e_aa7c_4cec0536f531_H

#include "../ailocal.h"
#include "goalentities.h"
#include "pool.h"
#include "../component.h"
#include <common/types/staticvector.h>
#include "../navigation/aasroutecache.h"
#include "worldstate.h"

class AiGoal {
	friend class Ai;
	friend class AiPlanner;
protected:
	Bot *const self;
	const char *name;
	const unsigned updatePeriod;
	int debugColor { 0 };
	float weight { 0.0f };

public:
	AiGoal( Bot *self_, const char *name_, unsigned updatePeriod_ )
		: self( self_ ), name( name_ ), updatePeriod( updatePeriod_ ) {}

	virtual ~AiGoal() = default;

	virtual void UpdateWeight( const WorldState &worldState ) = 0;
	virtual bool IsSatisfiedBy( const WorldState &worldState ) const = 0;
	virtual struct PlannerNode *GetWorldStateTransitions( const WorldState &worldState ) = 0;

	virtual void OnPlanBuildingStarted() {}
	virtual void OnPlanBuildingCompleted( const class AiActionRecord *planHead ) {}

	bool IsRelevant() const { return weight > 0; }

	// More important goals are first after sorting goals array
	bool operator<( const AiGoal &that ) const {
		return this->weight > that.weight;
	}

	int DebugColor() const { return debugColor; }

	const char *Name() const { return name; }
	unsigned UpdatePeriod() const { return updatePeriod; }
};

class AiActionRecord : public PoolItem {
	friend class AiBaseAction;
protected:
	Bot *const self;
	const char *name;

#ifndef _MSC_VER
	inline void Debug( const char *format, ... ) const __attribute__( ( format( printf, 2, 3 ) ) )
#else
	inline void Debug( _Printf_format_string_ const char *format, ... ) const
#endif
	{
		va_list va;
		va_start( va, format );
		AI_Debugv( name, format, va );
		va_end( va );
	}

public:
	AiActionRecord *nextInPlan { nullptr };

	AiActionRecord( PoolBase *pool_, Bot *self_, const char *name_ )
		: PoolItem( pool_ ), self( self_ ), name( name_ ) {}

	virtual void Activate() {
		Debug( "About to activate\n" );
	};

	virtual void Deactivate() {
		Debug( "About to deactivate\n" );
	};

	const char *Name() const { return name; }

	enum Status {
		INVALID,
		VALID,
		COMPLETED
	};

	virtual Status UpdateStatus( const WorldState &currWorldState ) = 0;
};

struct PlannerNode : PoolItem {
	// World state after applying an action
	WorldState worldState;
	// An action record to apply
	AiActionRecord *actionRecord { nullptr };
	// Used to reconstruct a plan
	PlannerNode *parent { nullptr };
	// Next in linked list of transitions for current node
	PlannerNode *nextTransition { nullptr };

	// Utilities for storing the node in a hash set
	PlannerNode *prevInHashBin { nullptr };
	PlannerNode *nextInHashBin { nullptr };

	const char *producedByAction { nullptr };

	// An A-star edge "distance"
	float transitionCost { std::numeric_limits<float>::max() };
	// An A-star node "G"
	float costSoFar { std::numeric_limits<float>::max() };
	// A priority queue parameter
	float heapCost { std::numeric_limits<float>::max() };
	// An utility for retrieval an actual index in heap array by a node value
	unsigned heapArrayIndex { std::numeric_limits<unsigned>::max() };

	// A hash of the associated world state (put here for optimal members alignment)
	uint32_t worldStateHash { 0 };

	inline PlannerNode( PoolBase *pool, Bot *self );

	~PlannerNode() override {
		if( actionRecord ) {
			actionRecord->DeleteSelf();
		}

#ifndef PUBLIC_BUILD
		// Prevent use-after-free.
		actionRecord = nullptr;
		parent = nullptr;
		nextTransition = nullptr;
		prevInHashBin = nullptr;
		nextInHashBin = nullptr;
#endif
	}
};

class AiAction {
	friend class Ai;
protected:
	Bot *self;
	const char *name;

#ifndef _MSC_VER
	inline void Debug( const char *format, ... ) const __attribute__( ( format( printf, 2, 3 ) ) )
#else
	inline void Debug( _Printf_format_string_ const char *format, ... ) const
#endif
	{
		va_list va;
		va_start( va, format );
		AI_Debugv( name, format, va );
		va_end( va );
	}

	PlannerNode *newNodeForRecord( AiActionRecord *record, const WorldState &worldState, float cost );
public:
	AiAction( Bot *self_, const char *name_ )
		: self( self_ ), name( name_ ) {}

	virtual ~AiAction() = default;

	const char *Name() const { return name; }

	virtual PlannerNode *TryApply( const WorldState &worldState ) = 0;
};

class AiPlanner : public AiComponent {
	friend class Ai;
	friend class AiManager;
	friend class AiBaseTeam;
	friend class AiGoal;
	friend class AiAction;
	friend class AiActionRecord;
	friend class PlanningModule;
public:
	static constexpr unsigned MAX_GOALS = 12;
	static constexpr unsigned MAX_ACTIONS = 36;

protected:
	Bot *const ai;

	AiActionRecord *planHead { nullptr };
	AiGoal *activeGoal { nullptr };
	int64_t nextActiveGoalUpdateAt { 0 };

	wsw::StaticVector<AiGoal *, MAX_GOALS> goals;
	wsw::StaticVector<AiAction *, MAX_ACTIONS> actions;

	static constexpr unsigned MAX_PLANNER_NODES = 384;
	Pool<PlannerNode, MAX_PLANNER_NODES> plannerNodesPool { "PlannerNodesPool" };

	explicit AiPlanner( Bot *ai_ ): ai( ai_ ) {}

	virtual void PrepareCurrWorldState( WorldState *worldState ) = 0;

	virtual bool ShouldSkipPlanning() const = 0;

	bool UpdateGoalAndPlan( const WorldState &currWorldState );

	bool FindNewGoalAndPlan( const WorldState &currWorldState );

	// Allowed to be overridden in a subclass for class-specific optimization purposes
	virtual AiActionRecord *BuildPlan( AiGoal *goal, const WorldState &startWorldState );

	AiActionRecord *ReconstructPlan( PlannerNode *lastNode ) const;

	void SetGoalAndPlan( AiGoal *goal_, AiActionRecord *planHead_ );

	virtual void BeforePlanning() {}
	virtual void AfterPlanning() {}
public:
	bool HasPlan() const { return planHead != nullptr; }

	void Update();

	void ClearGoalAndPlan();

	void DeletePlan( AiActionRecord *head );
};

#endif
