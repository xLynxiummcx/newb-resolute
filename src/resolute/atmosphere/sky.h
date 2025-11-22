float rayleighPhase(float mu) {
    return (3.0 / (16.0 * PI)) * (1.0 + mu * mu);
}

float hgPhase(float mu, float g) {
    return (1.0 / (4.0 * PI)) * ((1.0 - g * g) / pow(1.0 + g * g - 2.0 * g * mu, 1.5));
}

float opticaldepth(float k, float Vy) {
    return 1.0 - exp(-k * max(Vy, 0.0));
}
vec3 getSkyColor(vec3 dir, vec3 sunDir, float day, float night, float rain, float sunset, float sunrise, float snow, float time) {
    vec3 dayHorizon   = vec3(0.7, 0.9, 1.0);   // Soft sky blue horizon
    vec3 dayMid       = vec3(0.4, 0.7, 1.0);   // Medium blue
    vec3 dayZenith    = vec3(0.1, 0.3, 0.8);   // Deep blue zenith
    
    vec3 nightHorizon = vec3(0.1, 0.15, 0.25); // Dark blue horizon
    vec3 nightMid     = vec3(0.05, 0.08, 0.15); // Deeper blue
    vec3 nightZenith  = vec3(0.02, 0.03, 0.08); // Near-black zenith
    
    vec3 rainHorizon  = vec3(0.5, 0.55, 0.6);  // Grey-blue horizon
    vec3 rainMid      = vec3(0.4, 0.45, 0.5);  // Medium grey-blue
    vec3 rainZenith   = vec3(0.3, 0.35, 0.4);  // Dark grey-blue
    
    vec3 snowHorizon  = vec3(0.85, 0.9, 0.95); // Bright blue-white horizon
    vec3 snowMid      = vec3(0.75, 0.82, 0.9); // Medium blue-white
    vec3 snowZenith   = vec3(0.6, 0.7, 0.85);  // Soft blue zenith
    
    vec3 sunsetHorizon = vec3(1.0, 0.5, 0.3);  // Bright orange-red
    vec3 sunsetMid     = vec3(0.8, 0.4, 0.2);  // Medium orange
    vec3 sunsetZenith  = vec3(0.3, 0.2, 0.4);  // Purple zenith
    
    vec3 sunriseHorizon = vec3(1.0, 0.7, 0.4); // Soft orange
    vec3 sunriseMid     = vec3(0.8, 0.5, 0.3); // Medium orange
    vec3 sunriseZenith  = vec3(0.4, 0.3, 0.5); // Soft purple zenith


    float horizonBlend = smoothstep(0.0, 0.2, dir.y);    // Horizon to mid transition
    float midBlend = smoothstep(0.2, 0.6, dir.y);        // Mid to zenith transition
    

    vec3 dayGradient = mix(dayHorizon, dayMid, horizonBlend);
    dayGradient = mix(dayGradient, dayZenith, midBlend);
    
    vec3 nightGradient = mix(nightHorizon, nightMid, horizonBlend);
    nightGradient = mix(nightGradient, nightZenith, midBlend);
    
    vec3 rainGradient = mix(rainHorizon, rainMid, horizonBlend);
    rainGradient = mix(rainGradient, rainZenith, midBlend);
    
    vec3 snowGradient = mix(snowHorizon, snowMid, horizonBlend);
    snowGradient = mix(snowGradient, snowZenith, midBlend);
    
    vec3 sunsetGradient = mix(sunsetHorizon, sunsetMid, horizonBlend);
    sunsetGradient = mix(sunsetGradient, sunsetZenith, midBlend);
    
    vec3 sunriseGradient = mix(sunriseHorizon, sunriseMid, horizonBlend);
    sunriseGradient = mix(sunriseGradient, sunriseZenith, midBlend);


    vec3 base = mix(dayGradient, nightGradient, night);
    base = mix(base, rainGradient, rain);
    base = mix(base, snowGradient, snow);
    
    float sunHorizonFactor = max(sunDir.y, 0.0);
    base = mix(base, sunsetGradient, sunset * sunHorizonFactor);
    base = mix(base, sunriseGradient, sunrise * sunHorizonFactor);


    float scatterDot = max(dot(dir, sunDir), 0.0);
    
    float scatterIntensity = (sunset + sunrise) * 0.8 + day * 0.3;
    float scatter = pow(scatterDot, 8.0) * scatterIntensity * (1.0 - rain * 0.7);
    
    vec3 scatterColor = mix(
        vec3(1.0, 0.9, 0.8),
        vec3(1.0, 0.6, 0.3),
        max(sunset, sunrise)
    );
    base += scatter * scatterColor * (1.0 - night * 0.8);

    float sunGlow = pow(scatterDot, 64.0) * 3.0 * (1.0 - rain);
    vec3 glowColor = mix(
        vec3(1.0, 1.0, 0.9), 
        vec3(1.0, 0.7, 0.3),
        max(sunset, sunrise)
    );
    
    base += sunGlow * glowColor * (1.0 - night * 0.5);

    return base;
}

vec3 mainSkyRender(
    vec3 p,
    vec3 sunDir,
    float day, 
    float night,
    float rain,
    float sunset,
    float sunrise,
    float snow,
    out vec3 finalsunColor,
    float time
) {
    vec3 skyColor = getSkyColor(p, sunDir, day, night, rain, sunset, sunrise, snow, time);
    vec3 sunDay     = vec3(1.0, 0.9, 0.8);
    vec3 sunNight   = vec3(0.4, 0.6, 1.0);
    vec3 sunRain    = vec3(0.8, 0.85, 0.9);
    vec3 sunSunset  = vec3(1.0, 0.5, 0.2);
    vec3 sunSunrise = vec3(1.0, 0.7, 0.4);
    vec3 sunSnow    = vec3(1.0, 0.95, 0.9);
    vec3 sunColor = sunDay;
    sunColor = mix(sunColor, sunNight, night);
    sunColor = mix(sunColor, sunSunset, sunset);
    sunColor = mix(sunColor, sunSunrise, sunrise);
    sunColor = mix(sunColor, sunRain, rain);
    sunColor = mix(sunColor, sunSnow, snow);
    finalsunColor = sunColor;
    skyColor = pow(skyColor,vec3_splat(0.78));
    return skyColor;
}	

float getSunIntensity(vec3 sunDir, vec3 viewDir, float night) {
    float sunDot = dot(sunDir, viewDir);
    
    float core = smoothstep(0.98, 1.0, sunDot);
    
    float corona = pow(sunDot, 64.0) * max(0.8 - 0.7 * night, 0.0);
    float outerGlow = pow(sunDot, 8.0) * max(0.3 - 0.2 * night, 0.0);
    float sun = core + corona + outerGlow;
    
    return sun;
}
