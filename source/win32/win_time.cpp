#include <common/common.h>
#include <windows.h>

// TODO: Should we just use std::high_precision_clock for every platform? Its fairly good nowadays...

static LARGE_INTEGER hwTimerFrequency;
static LARGE_INTEGER startupTimestamp;

void Sys_InitTime() {
	(void)::QueryPerformanceFrequency( &hwTimerFrequency );
	(void)::QueryPerformanceCounter( &startupTimestamp );
}

int64_t Sys_Milliseconds() {
	LARGE_INTEGER counter;
	(void)::QueryPerformanceCounter( &counter );
	// Isn't really needed as the return value is 64-bit but should make stuff more robust
	counter.QuadPart -= startupTimestamp.QuadPart;
	counter.QuadPart *= 1000LL;
	counter.QuadPart /= hwTimerFrequency.QuadPart;
	return counter.QuadPart;
}

uint64_t Sys_Microseconds() {
	LARGE_INTEGER counter;
	(void)::QueryPerformanceCounter( &counter );
	counter.QuadPart -= startupTimestamp.QuadPart;
	counter.QuadPart *= 1000LL * 1000LL;
	counter.QuadPart /= hwTimerFrequency.QuadPart;
	return (uint64_t)counter.QuadPart;
}