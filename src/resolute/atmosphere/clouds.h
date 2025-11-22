
vec4 fastclouds(vec3 vdir, float time, vec3 sunDir, vec3 sky, vec3 suncol, float rain) {
    const float cloudScale = 1.6;
    const float driftSpeed = 0.05;
    const float shadowOffset = 0.25;
    vec2 uv = vdir.xz / max(0.25, vdir.y + 0.3);
    vec3 p = vec3(uv * cloudScale, time * driftSpeed);
    
    float base = fbm3D(p);
    float detail = fbm3D(p * 2.5 + 5.0);
    float density = base;

    density = smoothstep(0.35-1.0*rain, 0.8+0.1*rain, density);
    density = pow(density, 0.9+1.0*rain);

    vec3 shadowPos = p + sunDir * shadowOffset;
    float shadowBase = fbm3D(shadowPos);
    float shadowDetail = fbm3D(shadowPos * 2.5 + 5.0);
    float shadowDensity = shadowBase;
    
    shadowDensity = smoothstep(0.35-1.0*rain, 0.8+0.1*rain, shadowDensity);
    shadowDensity = pow(shadowDensity, 0.9+1.0*rain);

    float shadowFactor = 1.0 - shadowDensity;
    vec3 color = mix(sky, suncol, shadowFactor);

    float heightFade = clamp((vdir.y + 0.1) * 2.5, 0.0, 1.0);
    float horizon = smoothstep(0.0, 0.3, abs(vdir.y));
    float alpha = density * heightFade * horizon;

    return vec4(color, alpha);
}