float lightningFlash(float t) {
    float flashFreq = 0.15;
    float phase = fract(t * flashFreq);
    
    float seed = floor(t * flashFreq);
    float r = fract(sin(seed * 43758.5453) * 12.9898);

    float trigger = step(0.9, r); 

    float flash = exp(-20.0 * phase) * trigger;
    
    return flash;
}

float lightning(vec2 uv, vec2 start, vec2 end, float seed) {
    vec2 dir = end - start;
    float len = length(dir);
    dir = normalize(dir);
    vec2 perp = vec2(-dir.y, dir.x);

    vec2 toPoint = uv - start;
    float along = dot(toPoint, dir);
    float aside = dot(toPoint, perp);

    if (along < 0.0 || along > len) return 0.0;

    float t = along / len;
    float zigzag = noisegod(vec2(t * 12.0 + seed * 10.0, seed * 5.0)) - 0.5;
    zigzag += noisegod(vec2(t * 30.0 + seed * 15.0, seed * 7.0)) * 0.3;
    float offset = zigzag * 0.15 * (sin(t * 3.14159) * 0.5 + 0.5);

    float dist = abs(aside - offset);
    float thickness = 0.003 * (1.0 - t * 0.3);
    float core = smoothstep(thickness, 0.0, dist);
    float glow = smoothstep(thickness * 8.0, 0.0, dist) * 0.4;

    return core + glow;
}

float branch(vec2 uv, vec2 start, vec2 end, float seed, float intensity) {
    vec2 dir = end - start;
    float len = length(dir);
    dir = normalize(dir);
    vec2 perp = vec2(-dir.y, dir.x);

    vec2 toPoint = uv - start;
    float along = dot(toPoint, dir);
    float aside = dot(toPoint, perp);

    if (along < 0.0 || along > len) return 0.0;

    float t = along / len;
    float zigzag = (noisegod(vec2(t * 15.0 + seed, seed * 3.0)) - 0.5) * 0.1;
    float offset = zigzag * (1.0 - t * 0.5);

    float dist = abs(aside - offset);
    float thickness = 0.002 * intensity;
    float core = smoothstep(thickness, 0.0, dist);
    float glow = smoothstep(thickness * 5.0, 0.0, dist) * 0.3 * intensity;

    return (core + glow) * (1.0 - t * 0.4);
}
