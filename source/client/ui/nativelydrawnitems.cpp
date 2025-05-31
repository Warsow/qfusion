#include "nativelydrawnitems.h"
#include <client/renderer/ref.h>

#include <QQmlProperty>

#include <common/common.h>
#include <client/cgame/cg_boneposes.h>

namespace wsw::ui {

wsw::PodVector<shader_s *> NativelyDrawn::s_materialsToRecycle;

void NativelyDrawn::recycleResourcesInMainContext() {
	for( auto *material: s_materialsToRecycle ) {
		R_ReleaseExplicitlyManaged2DMaterial( material );
	}
	s_materialsToRecycle.clear();
}

NativelyDrawnImage::NativelyDrawnImage( QQuickItem *parent )
	: QQuickItem( parent ) {
	m_selfAsItem = this;
}

NativelyDrawnImage::~NativelyDrawnImage() {
	if( m_material ) {
		s_materialsToRecycle.push_back( m_material );
	}
}

void NativelyDrawnImage::setNativeZ( int nativeZ ) {
	if( m_nativeZ != nativeZ ) {
		m_nativeZ = nativeZ;
		Q_EMIT nativeZChanged( nativeZ );
	}
}

void NativelyDrawnImage::setMaterialName( const QString &materialName ) {
	if( m_materialName != materialName ) {
		m_materialName = materialName;
		m_reloadRequestMask |= ReloadMaterial;
		Q_EMIT materialNameChanged( materialName );
	}
}

void NativelyDrawnImage::setDesiredSize( const QSize &size ) {
	if( m_desiredSize != size ) {
		m_desiredSize = size;
		m_reloadRequestMask |= ChangeOtherOptions;
		Q_EMIT desiredSizeChanged( size );
	}
}

void NativelyDrawnImage::setUseOutlineEffect( bool useOutlineEffect ) {
	if( m_useOutlineEffect != useOutlineEffect ) {
		m_useOutlineEffect = useOutlineEffect;
		m_reloadRequestMask |= ChangeOtherOptions;
		Q_EMIT useOutlineEffectChanged( useOutlineEffect );
	}
}

void NativelyDrawnImage::setFitSizeForCrispness( bool fitSizeForCrispness ) {
	if( m_fitSizeForCrispness != fitSizeForCrispness ) {
		m_fitSizeForCrispness = fitSizeForCrispness;
		m_reloadRequestMask |= ChangeOtherOptions;
		Q_EMIT fitSizeForCrispnessChanged( fitSizeForCrispness );
	}
}

void NativelyDrawnImage::setBorderWidth( int borderWidth ) {
	if( m_borderWidth != borderWidth ) {
		m_borderWidth = borderWidth;
		m_reloadRequestMask |= ChangeOtherOptions;
		Q_EMIT borderWidthChanged( borderWidth );
	}
}

bool NativelyDrawnImage::isLoaded() const {
	return m_material && m_isMaterialLoaded;
}

void NativelyDrawnImage::updateSourceSize( int w, int h ) {
	const bool hasChanges = m_sourceSize.width() != w || m_sourceSize.height() != h;
	m_sourceSize.setWidth( w );
	m_sourceSize.setHeight( h );
	if( hasChanges ) {
		Q_EMIT sourceSizeChanged( m_sourceSize );
	}
}

void NativelyDrawnImage::reloadIfNeeded( int pixelsPerLogicalUnit ) {
	if( !m_reloadRequestMask ) {
		return;
	}

	m_reloadRequestMask = 0;

	const bool wasLoaded = this->isLoaded();
	const QByteArray nameBytes( m_materialName.toUtf8() );

	if( !m_material ) {
		m_material = R_CreateExplicitlyManaged2DMaterial();
	}

	ImageOptions options;
	options.fitSizeForCrispness = true;
	options.useOutlineEffect    = m_useOutlineEffect;
	options.fitSizeForCrispness = m_fitSizeForCrispness;
	options.borderWidth         = pixelsPerLogicalUnit * m_borderWidth;
	if( m_desiredSize.isValid() && !m_desiredSize.isEmpty() ) {
		options.setDesiredSize( pixelsPerLogicalUnit * m_desiredSize.width(), pixelsPerLogicalUnit * m_desiredSize.height() );
		const int minSide   = wsw::min( m_desiredSize.width(), m_desiredSize.height() );
		options.borderWidth = pixelsPerLogicalUnit * wsw::clamp( m_borderWidth, 0, minSide / 2 - 1 );
	}

	m_isMaterialLoaded = R_UpdateExplicitlyManaged2DMaterialImage( m_material, nameBytes, options );

	const bool isLoaded = this->isLoaded();
	if( wasLoaded != isLoaded ) {
		Q_EMIT isLoadedChanged( isLoaded );
	}

	int w = 0, h = 0;
	if( isLoaded ) {
		if( const auto maybeDimensions = R_GetShaderDimensions( m_material ) ) {
			w = maybeDimensions->first / pixelsPerLogicalUnit;
			h = maybeDimensions->second / pixelsPerLogicalUnit;
		}
	}
	updateSourceSize( w, h );
}

void NativelyDrawnImage::drawSelfNatively( int64_t, int64_t, int pixelsPerLogicalUnit ) {
	reloadIfNeeded( pixelsPerLogicalUnit );

	if( isLoaded() ) {
		R_Set2DMode( true );

		const float opacity = QQmlProperty::read( m_selfAsItem, "opacity" ).toFloat();
		const vec4_t color {
			(float)m_color.redF(), (float)m_color.greenF(), (float)m_color.blueF(), opacity * (float)m_color.alphaF()
		};

		const QPointF globalPoint( mapToGlobal( QPointF( x(), y() ) ) );
		const auto qmlX = (int)globalPoint.x(), qmlY = (int)globalPoint.y();

		// TODO: Check rounding
		// TODO: Setup scissor if the clip rect is defined

		assert( m_sourceSize.isValid() );

		// Check whether the bitmap size is specified
		if( m_desiredSize.isValid() ) {
			const int x = pixelsPerLogicalUnit * ( qmlX + (int)width() / 2 );
			const int y = pixelsPerLogicalUnit * ( qmlY + (int)height() / 2 );
			const int w = pixelsPerLogicalUnit * m_sourceSize.width();
			const int h = pixelsPerLogicalUnit * m_sourceSize.height();
			R_DrawStretchPic( x - w / 2, y - w / 2, w, h, 0.0f, 0.0f, 1.0f, 1.0f, color, m_material );
		} else {
			const int x = pixelsPerLogicalUnit * qmlX;
			const int y = pixelsPerLogicalUnit * qmlY;
			const int w = pixelsPerLogicalUnit * (int)width();
			const int h = pixelsPerLogicalUnit * (int)height();
			R_DrawStretchPic( x, y, w, h, 0.0f, 0.0f, 1.0f, 1.0f, color, m_material );
		}

		R_Set2DMode( false );
	}
}

NativelyDrawnModel::NativelyDrawnModel( QQuickItem *parent )
	: QQuickItem( parent ) {
	m_selfAsItem = this;
	updateViewAxis();
}

void NativelyDrawnModel::setNativeZ( int nativeZ ) {
	if( m_nativeZ != nativeZ ) {
		m_nativeZ = nativeZ;
		Q_EMIT nativeZChanged();
	}
}

void NativelyDrawnModel::setModelName( const QString &modelName ) {
	if( m_modelName != modelName ) {
		m_modelName = modelName;
		m_reloadRequestMask |= ReloadModel;
		Q_EMIT modelNameChanged();
	}
}

void NativelyDrawnModel::setSkinName( const QString &skinName ) {
	if( m_skinName != skinName ) {
		m_skinName = skinName;
		m_reloadRequestMask |= ReloadSkin;
		Q_EMIT skinNameChanged();
	}
}

void NativelyDrawnModel::setModelOrigin( const QVector3D &modelOrigin ) {
	if( m_modelOrigin != modelOrigin ) {
		m_modelOrigin = modelOrigin;
		Q_EMIT modelOriginChanged( modelOrigin );
	}
}

void NativelyDrawnModel::setViewOrigin( const QVector3D &viewOrigin ) {
	if( m_viewOrigin != viewOrigin ) {
		m_viewOrigin = viewOrigin;
		m_needsViewAxisUpdate = true;
		Q_EMIT viewOriginChanged( viewOrigin );
	}
}

void NativelyDrawnModel::setViewTarget( const QVector3D &viewTarget ) {
	if( m_viewTarget != viewTarget ) {
		m_viewTarget = viewTarget;
		m_needsViewAxisUpdate = true;
		Q_EMIT viewTargetChanged( viewTarget );
	}
}

void NativelyDrawnModel::setRotationSpeed( qreal rotationSpeed ) {
	if( m_rotationSpeed != rotationSpeed ) {
		m_rotationSpeed = rotationSpeed;
		Q_EMIT rotationSpeedChanged( rotationSpeed );
	}
}

void NativelyDrawnModel::setViewFov( qreal viewFov ) {
	if( m_viewFov != viewFov ) {
		m_viewFov = viewFov;
		Q_EMIT viewFovChanged( viewFov );
	}
}

void NativelyDrawnModel::setDesiredModelHeight( qreal desiredModelHeight ) {
	if( m_desiredModelHeight != desiredModelHeight ) {
		m_desiredModelHeight = desiredModelHeight;
		Q_EMIT desiredModelHeightChanged( desiredModelHeight );
	}
}

void NativelyDrawnModel::setModelColor( const QColor &modelColor ) {
	if( m_modelColor != modelColor ) {
		m_modelColor = modelColor;
		m_transitionScale = 0.0f;
		Q_EMIT modelColorChanged( modelColor );
	}
}

void NativelyDrawnModel::setOutlineColor( const QColor &outlineColor ) {
	if( m_outlineColor != outlineColor ) {
		m_outlineColor = outlineColor;
		Q_EMIT outlineColorChanged( outlineColor );
	}
}

void NativelyDrawnModel::setOutlineHeight( qreal outlineHeight ) {
	if( m_outlineHeight != outlineHeight ) {
		m_outlineHeight = outlineHeight;
		Q_EMIT outlineHeightChanged( outlineHeight );
	}
}

bool NativelyDrawnModel::isLoaded() const {
	return m_model != nullptr;
}

void NativelyDrawnModel::reloadIfNeeded() {
	if( m_reloadRequestMask ) {
		m_transitionScale = 0.0f;
	}

	if( m_reloadRequestMask & ReloadModel ) {
		const bool wasLoaded = m_model != nullptr;
		m_model = R_RegisterModel( m_modelName.toUtf8().constData() );
		const bool isLoaded = m_model != nullptr;
		if( wasLoaded != isLoaded ) {
			Q_EMIT isLoadedChanged( isLoaded );
		}
	}

	if( m_reloadRequestMask & ReloadSkin ) {
		m_skinFile = R_RegisterSkinFile( m_skinName.toUtf8().constData() );
	}

	m_reloadRequestMask = 0;
}

void NativelyDrawnModel::updateViewAxis() {
	vec3_t dir { 1.0f, 0.0f, 0.0f };
	QVector3D diff( m_viewTarget - m_viewOrigin );
	if( const float len = diff.lengthSquared(); len > 1.0f ) {
		diff *= 1.0f / std::sqrt( len );
		VectorCopy( diff, dir );
	}
	vec3_t angles;
	VecToAngles( dir, angles );
	AnglesToAxis( angles, m_viewAxis );
}

static inline void setByteColorFromQColor( uint8_t *byteColor, const QColor &color ) {
	byteColor[0] = (uint8_t)( color.redF() * 255.0 );
	byteColor[1] = (uint8_t)( color.greenF() * 255.0 );
	byteColor[2] = (uint8_t)( color.blueF() * 255.0 );
	byteColor[3] = (uint8_t)( color.alphaF() * 255.0 );
}

void NativelyDrawnModel::drawSelfNatively( int64_t, int64_t timeDelta, int pixelsPerLogicalUnit ) {
	reloadIfNeeded();

	if( !m_model ) {
		return;
	}

	if( m_needsViewAxisUpdate ) {
		updateViewAxis();
		m_needsViewAxisUpdate = false;
	}

	const QRect rect( mapRectToScene( m_selfAsItem->boundingRect() ).toRect() );
	const int x      = pixelsPerLogicalUnit * rect.x();
	const int y      = pixelsPerLogicalUnit * rect.y();
	const int width  = pixelsPerLogicalUnit * rect.width();
	const int height = pixelsPerLogicalUnit * rect.height();

	R_Set2DMode( false );

	refdef_t refdef {};
	entity_t entity {};

	refdef.x = x;
	refdef.y = y;
	refdef.width = width;
	refdef.height = height;

	refdef.scissor_x = x;
	refdef.scissor_y = y;
	refdef.scissor_width = width;
	refdef.scissor_height = height;

	refdef.fov_x = (float)m_viewFov;
	refdef.fov_y = CalcFov( refdef.fov_x, (float)refdef.width, (float)refdef.height );
	refdef.vieworg[0] = (float)m_viewOrigin.x();
	refdef.vieworg[1] = (float)m_viewOrigin.y();
	refdef.vieworg[2] = (float)m_viewOrigin.z();
	Matrix3_Copy( m_viewAxis, refdef.viewaxis );

	refdef.rdflags = RDF_NOWORLDMODEL;
	refdef.minLight = 0.7f;

	entity.model = m_model;
	entity.customSkin = m_skinFile;
	entity.frame = entity.oldframe = 1;
	entity.renderfx = RF_NOSHADOW | RF_FORCENOLOD | RF_MINLIGHT;
	entity.outlineHeight = (float)( m_outlineHeight * pixelsPerLogicalUnit );
	entity.origin[0] = (float)m_modelOrigin.x();
	entity.origin[1] = (float)m_modelOrigin.y();
	entity.origin[2] = (float)m_modelOrigin.z();
	VectorCopy( entity.origin, entity.origin2 );

	setByteColorFromQColor( entity.shaderRGBA, m_modelColor );
	setByteColorFromQColor( entity.outlineRGBA, m_outlineColor );

	entity.scale = 1.0;
	// Hacks to show player models with approximately the same height
	if( m_desiredModelHeight > 0 ) {
		vec3_t modelMins, modelMaxs;
		R_ModelFrameBounds( m_model, 0, modelMins, modelMaxs );
		if( const float modelHeight = modelMaxs[2] - modelMins[2]; height > 0 ) {
			entity.scale = (float)m_desiredModelHeight / modelHeight;
		}
	}

	// Scale units per second
	constexpr float transitionSpeed = 1.5f;
	const auto timeDeltaSeconds = 1e-3f * (float)timeDelta;
	m_transitionScale = wsw::min( m_transitionScale + transitionSpeed * timeDeltaSeconds, 1.0f );
	entity.scale *= m_transitionScale;

	if( const auto rotationSpeed = (float)m_rotationSpeed; rotationSpeed != 0.0f ) {
		vec3_t angles { 0, m_rotationAngle, 0 };
		AngleVectors( angles, &entity.axis[0], &entity.axis[3], &entity.axis[6] );
		VectorInverse( &entity.axis[3] );
		m_rotationAngle = anglemod( m_rotationAngle + timeDeltaSeconds * rotationSpeed );
	} else {
		Matrix3_Copy( axis_identity, entity.axis );
	}

	BeginDrawingScenes();
	DrawSceneRequest *drawSceneRequest = CreateDrawSceneRequest( refdef );
	CG_SetBoneposesForTemporaryEntity( &entity );
	drawSceneRequest->addEntity( &entity );
	ExecuteSingleDrawSceneRequestNonSpeedCritical( drawSceneRequest );
	EndDrawingScenes();
}

}