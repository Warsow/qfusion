myhalf LinearFromsRGB(myhalf c)
{
	myhalf falsePathVal = c * (1.0f / 12.92f);
	myhalf truePathVal = pow((c + 0.055f) * (1.0f/1.055f), 2.4f);
	return mix(falsePathVal, truePathVal, c > 0.04045f);
}

myhalf3 LinearFromsRGB(myhalf3 v)
{
	myhalf3 falsePathVal = v * myhalf3(1.0f / 12.92f);
	myhalf3 truePathVal = pow((v + myhalf3(0.055f)) * myhalf3(1.0f/1.055f), myhalf3(2.4f));
	return mix(falsePathVal, truePathVal, greaterThan(v, myhalf3(0.04045f)));
}

myhalf4 LinearFromsRGB(myhalf4 v)
{
	return myhalf4(LinearFromsRGB(v.rgb), v.a);
}

myhalf sRGBFromLinear(myhalf c)
{
	myhalf falsePathVal = c * 12.92f;
	myhalf truePathVal = 1.055f * pow(c, 1.0f/2.4f) - 0.055f;
	return mix(falsePathVal, truePathVal, c >= 0.0031308f);
}

myhalf3 sRGBFromLinear(myhalf3 v)
{
	myhalf3 falsePathVal = v * myhalf3(12.92f);
	myhalf3 truePathVal = myhalf3(1.055f) * pow(v, myhalf3(1.0f/2.4f)) - myhalf3(0.055f);
	return mix(falsePathVal, truePathVal, greaterThanEqual(v, myhalf3(0.0031308f)));
}

myhalf4 sRGBFromLinear(myhalf4 v)
{
	return myhalf4(sRGBFromLinear(v.rgb), v.a);
}

#ifdef APPLY_SRGB2LINEAR
# define LinearColor(c) LinearFromsRGB(c)
#else
# define LinearColor(c) (c)
#endif

#ifdef APPLY_LINEAR2SRGB
# define sRGBColor(c) sRGBFromLinear(c)
#else
# define sRGBColor(c) (c)
#endif

#if defined(APPLY_FOG_COLOR)
#define APPLY_ENV_MODULATE_COLOR
#else

#if defined(APPLY_RGB_DISTANCERAMP) || defined(APPLY_RGB_CONST) || defined(APPLY_RGB_VERTEX) || defined(APPLY_RGB_ONE_MINUS_VERTEX) || defined(APPLY_RGB_GEN_DIFFUSELIGHT)
#define APPLY_ENV_MODULATE_COLOR
#else

#if defined(APPLY_ALPHA_DISTANCERAMP) || defined(APPLY_ALPHA_CONST) || defined(APPLY_ALPHA_VERTEX) || defined(APPLY_ALPHA_ONE_MINUS_VERTEX)
#define APPLY_ENV_MODULATE_COLOR
#endif

#endif

#endif
