#ifndef WSW_16e4512a_b837_4c46_9141_bead40481a00_H
#define WSW_16e4512a_b837_4c46_9141_bead40481a00_H

#include "../ailocal.h"

class Bot;
class MovementSubsystem;
class PredictionContext;

class MovementScript {
public:
	enum Status {
		COMPLETED,
		PENDING,
		INVALID
	};

protected:
	const Bot *const bot;
	MovementSubsystem *const m_subsystem;
	int64_t activatedAt;
	Status status;
	int debugColor;

	void Activate() {
		activatedAt = level.time;
		status = PENDING;
	}

	// A convenient shorthand for returning from TryDeactivate()
	bool DeactivateWithStatus( Status status_ ) {
		assert( status_ != PENDING );
		this->status = status_;
		return true;
	}
public:
	MovementScript( const Bot *bot_, MovementSubsystem *subsystem, int debugColor_ )
		: bot( bot_ ), m_subsystem( subsystem ), activatedAt( 0 ), status( COMPLETED ), debugColor( debugColor_ ) {}

	bool IsActive() const { return status == PENDING; }

	int DebugColor()  const { return debugColor; }

	virtual ~MovementScript() = default;

	virtual bool TryDeactivate( PredictionContext *context = nullptr ) = 0;

	virtual void SetupMovement( PredictionContext *context ) = 0;
};

#endif
