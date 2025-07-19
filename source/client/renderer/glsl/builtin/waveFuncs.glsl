#ifndef M_TWOPI
#define M_TWOPI 6.28318530717958647692
#endif

#ifndef WAVE_SIN
float QF_WaveFunc_Sin(float x)
{
    return sin(fract(x) * M_TWOPI);
}
float QF_WaveFunc_Triangle(float x)
{
    x = fract(x);
    return step(x, 0.25) * x * 4.0 + (2.0 - 4.0 * step(0.25, x) * step(x, 0.75) * x) + ((step(0.75, x) * x - 0.75) * 4.0 - 1.0);
}
float QF_WaveFunc_Square(float x)
{
    return step(fract(x), 0.5) * 2.0 - 1.0;
}
float QF_WaveFunc_Sawtooth(float x)
{
    return fract(x);
}
float QF_WaveFunc_InverseSawtooth(float x)
{
    return 1.0 - fract(x);
}

#define WAVE_SIN(time,base,amplitude,phase,freq) (((base)+(amplitude)*QF_WaveFunc_Sin((phase)+(time)*(freq))))
#define WAVE_TRIANGLE(time,base,amplitude,phase,freq) (((base)+(amplitude)*QF_WaveFunc_Triangle((phase)+(time)*(freq))))
#define WAVE_SQUARE(time,base,amplitude,phase,freq) (((base)+(amplitude)*QF_WaveFunc_Square((phase)+(time)*(freq))))
#define WAVE_SAWTOOTH(time,base,amplitude,phase,freq) (((base)+(amplitude)*QF_WaveFunc_Sawtooth((phase)+(time)*(freq))))
#define WAVE_INVERSESAWTOOTH(time,base,amplitude,phase,freq) (((base)+(amplitude)*QF_WaveFunc_InverseSawtooth((phase)+(time)*(freq))))
#endif