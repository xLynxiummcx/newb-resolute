$input v_texcoord0, mj_pl, mj_pos

#include <bgfx_shader.sh>
#include <newb/config.h>
#include <newb/functions/tonemap.h>

uniform vec4 SunMoonColor;
SAMPLER2D_AUTOREG(s_SunMoonTexture);

uniform vec4 FogAndDistanceControl;
uniform vec4 FogColor;
uniform vec4 ViewPositionAndTime;

float noise(float t) {
    return fract(cos(t) * 3800.0);
}
vec3 generateLensFlare(vec2 uv, vec2 flarePosition) {
    vec2 mainVector = uv - flarePosition;
    vec2 distortedUV = uv * (length(uv));
    
    highp float intensity = 1.5;
    float angle = atan2(mainVector.y, mainVector.x);
    float distance = length(uv);
    distance = pow(distance, 0.01);
    float noiseValue = noise(0.0);
    
    // Central glow effect
    float centralGlow = 1.0 / (length(uv - 12.0) * 16.0 + 1.0) * 2.0;
    centralGlow = centralGlow * (sin((noiseValue * 2.0) * 12.0) * 0.1 + distance * 0.1 + 0.8);

    // Circular flare elements (multiple layers)
    float flare1 = max(1.0 / (1.0 + 32.0 * pow(length(distortedUV + 0.8 * flarePosition), 2.0)), 0.0) * 0.25;
    float flare2 = max(1.0 / (1.0 + 32.0 * pow(length(distortedUV + 0.85 * flarePosition), 2.0)), 0.0) * 0.23;
    float flare3 = max(1.0 / (1.0 + 32.0 * pow(length(distortedUV + 0.9 * flarePosition), 2.0)), 0.0) * 0.21;
    
    vec2 mixedUV = mix(uv, distortedUV, -0.5);
    
    // Additional flare layers
    float flare4 = max(0.02 - pow(length(mixedUV + 0.45 * flarePosition), 2.4), 0.0) * 6.0;
    float flare5 = max(0.02 - pow(length(mixedUV + 0.5 * flarePosition), 2.4), 0.0) * 5.0;
    float flare6 = max(0.02 - pow(length(mixedUV + 0.55 * flarePosition), 2.4), 0.0) * 3.0;
    
    mixedUV = mix(uv, distortedUV, -0.4);
    
    float flare7 = max(0.02 - pow(length(mixedUV + 0.3 * flarePosition), 5.5), 0.0) * 2.0;
    float flare8 = max(0.02 - pow(length(mixedUV + 0.5 * flarePosition), 5.5), 0.0) * 2.0;
    float flare9 = max(0.01 - pow(length(mixedUV + 0.7 * flarePosition), 5.5), 0.0) * 2.0;
    
    mixedUV = mix(uv, distortedUV, -0.5);
    
    float flare10 = max(0.02 - pow(length(mixedUV + 0.1 * flarePosition), 1.6), 0.0) * 6.0;
    float flare11 = max(0.01 - pow(length(mixedUV + 0.125 * flarePosition), 1.6), 0.0) * 3.0;
    float flare12 = max(0.01 - pow(length(mixedUV + 0.15 * flarePosition), 1.6), 0.0) * 5.0;
    
    highp vec3 flareColor = vec3_splat(0.0);
    flareColor.r += flare1 + flare4 + flare7 + flare10 + centralGlow;
    flareColor.g += flare2 + flare5 + flare8 + flare11 + centralGlow;
    flareColor.b += flare3 + flare6 + flare9 + flare12 + centralGlow;
    
    return flareColor * intensity;
}

vec3 colorCorrect(vec3 color, float factor, float factor2) {
    float luminance = color.r + color.g + color.b;
    return mix(color, vec3(luminance) * factor, luminance * factor2);
}

void main() {
    vec4 finalColor = vec4_splat(0.0);
    float distanceToCamera = length(-mj_pos);
    
    float rainFactor = (1.0 - pow(FogAndDistanceControl.y, 11.0));
    float duskFactor = pow(clamp(1.0 - FogColor.b * 1.2, 0.0, 1.0), 0.5);
    
    float distanceFade = (0.1 - clamp(distanceToCamera, 0.0, 0.1)) * 10.0;
    finalColor = mix(vec4_splat(0.0), finalColor, distanceFade);
    
    vec4 fogColor = FogColor;
    fogColor.rgb = max(fogColor.rgb, 0.4);
    fogColor.a *= 1.0 - rainFactor;
    finalColor.a = min(fogColor.a, 0.5);
    
    float atmosphereFade = 0.2 - clamp(distanceToCamera / FogAndDistanceControl.z * 200.0, 0.0, 0.2);
    vec3 atmosphereColor = mix(vec3_splat(1.0), vec3(1.0, 0.5, 0.0), duskFactor) * atmosphereFade;
    
    vec2 flareUV = -mj_pl.xz * 0.1;
    vec3 lensFlareColor = vec3(1.4, 1.2, 1.0) * generateLensFlare(mj_pos.xz * 16.0, flareUV) * 2.0;
    lensFlareColor = colorCorrect(lensFlareColor, 0.5, 0.1) * 0.5;
    
    finalColor.rgb += lensFlareColor + atmosphereColor;
    finalColor.a += length(lensFlareColor) * fogColor.a;
    
    gl_FragColor = clamp(finalColor * 0.5, 0.0, 1.0);
}