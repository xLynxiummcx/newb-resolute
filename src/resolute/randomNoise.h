float hash13(vec3 p3) {
    p3 = fract(p3 * 0.1031);
    p3 += dot(p3, p3.zyx + 31.32);
    return fract((p3.x + p3.y) * p3.z);
}
float hashlava(vec2 p){
    p = 50.0 * fract(p * 0.3183099 + vec2(0.71,0.113));
    return fract(p.x * p.y * 95.4337);
}
float hash12(vec2 p) {
    const vec3 K = vec3(0.3183099, 0.3678794, 43758.5453); // 1/pi, 1/e, prime-ish
    vec3 x = fract(vec3(p.xyx) * K.x + K.y);
    x += dot(x, x.yzx + 19.19);
    return fract((x.x + x.y) * x.z);
}
float valueNoiseTile(vec2 uv, float period){
    // uv in [0..1], period in pixels will tile over period
    vec2 p = uv * period;
    vec2 i = floor(p);
    vec2 f = fract(p);
    // wrap indexes (tile)
    vec2 i00 = mod(i, period);
    vec2 i10 = mod(i + vec2(1.0, 0.0), period);
    vec2 i01 = mod(i + vec2(0.0, 1.0), period);
    vec2 i11 = mod(i + vec2(1.0, 1.0), period);
    float a = hashlava(i00);
    float b = hashlava(i10);
    float c = hashlava(i01);
    float d = hashlava(i11);
    vec2 u = f*f*(3.0-2.0*f);
    float ab = mix(a,b,u.x);
    float cd = mix(c,d,u.x);
    return mix(ab,cd,u.y);
}

float fractalTile(vec2 uv){
    float n = 0.0;
    float amp = 0.6;
    float freq = 16.0; // base period
    for(int i=0;i<5;i++){
        n += amp * valueNoiseTile(uv, freq);
        freq *= 2.0;
        amp *= 0.55;
    }
    return clamp(n, 0.0, 1.0);
}


float noisegod(vec2 p) {
    vec2 i = floor(p);
    vec2 f = fract(p);

    // Interpolation curve
    vec2 u = f * f * (3.0 - 2.0 * f);

    // Noise at corners
    float a = hash12(i + vec2(0.0, 0.0));
    float b = hash12(i + vec2(1.0, 0.0));
    float c = hash12(i + vec2(0.0, 1.0));
    float d = hash12(i + vec2(1.0, 1.0));

    // Bilinear interpolation
    return mix(mix(a, b, u.x), mix(c, d, u.x), u.y);
}
float noise(vec2 p) {
    vec2 i = floor(p);
    vec2 f = fract(p);

    // Interpolation curve
    vec2 u = f * f * (3.0 - 2.0 * f);

    // Noise at corners
    float a = hash12(i + vec2(0.0, 0.0));
    float b = hash12(i + vec2(1.0, 0.0));
    float c = hash12(i + vec2(0.0, 1.0));
    float d = hash12(i + vec2(1.0, 1.0));

    // Bilinear interpolation
    return mix(mix(a, b, u.x), mix(c, d, u.x), u.y);
}
float fbm(vec2 p) {
    float value = 0.0;
    float amplitude = 0.5;
    float frequency = 1.0;

    for (int i = 0; i < 5; i++) {  // 5 octaves
        value += amplitude * noisegod(p * frequency);
        frequency *= 2.0;
        amplitude *= 0.5;
    }
    return value;
}
// Simple 3D noise function for fog
float hashfog(vec3 p) {
    p = fract(p * 0.3183099 + 0.1);
    p *= 17.0;
    return fract(p.x * p.y * p.z * (p.x + p.y + p.z));
}

float noise3Dfog(vec3 p) {
    vec3 i = floor(p);
    vec3 f = fract(p);
    f = f * f * (3.0 - 2.0 * f); // smoothstep interpolation
    
    return mix(
        mix(mix(hashfog(i + vec3(0,0,0)), hashfog(i + vec3(1,0,0)), f.x),
            mix(hashfog(i + vec3(0,1,0)), hashfog(i + vec3(1,1,0)), f.x), f.y),
        mix(mix(hashfog(i + vec3(0,0,1)), hashfog(i + vec3(1,0,1)), f.x),
            mix(hashfog(i + vec3(0,1,1)), hashfog(i + vec3(1,1,1)), f.x), f.y),
        f.z
    );
}
float noise(vec3 p) {
    vec3 i = floor(p);
    vec3 f = fract(p);
    f = f * f * (3.0 - 2.0 * f); // smoothstep interpolation
    
    return mix(
        mix(mix(hashfog(i + vec3(0,0,0)), hashfog(i + vec3(1,0,0)), f.x),
            mix(hashfog(i + vec3(0,1,0)), hashfog(i + vec3(1,1,0)), f.x), f.y),
        mix(mix(hashfog(i + vec3(0,0,1)), hashfog(i + vec3(1,0,1)), f.x),
            mix(hashfog(i + vec3(0,1,1)), hashfog(i + vec3(1,1,1)), f.x), f.y),
        f.z
    );
}

// Fractal Brownian Motion for more detailed noise
float fbmfog(vec3 p) {
    float value = 0.0;
    float amplitude = 0.5;
    float frequency = 1.0;
    
    // 3 octaves for performance (add more for detail)
    for(int i = 0; i < 3; i++) {
        value += amplitude * noise3Dfog(p * frequency);
        frequency *= 2.0;
        amplitude *= 0.5;
    }
    
    return value;
}


float valueNoise(vec2 p) {
    vec2 i = floor(p);
    vec2 f = fract(p);

    vec2 u = f * f * f * (f * (f * 6.0 - 15.0) + 10.0);

    float a = hash12(i + vec2(0.0, 0.0));
    float b = hash12(i + vec2(1.0, 0.0));
    float c = hash12(i + vec2(0.0, 1.0));
    float d = hash12(i + vec2(1.0, 1.0));

    return mix(mix(a, b, u.x), mix(c, d, u.x), u.y);
}

float hash(vec3 p) {
    p = fract(p * 0.3183099 + vec3(0.1, 0.1, 0.1));
    p *= 17.0;
    return fract(p.x * p.y * p.z * (p.x + p.y + p.z));
}

float cnoise3D(vec3 p) {
    vec3 i = floor(p);
    vec3 f = fract(p);
    f = f * f * (3.0 - 2.0 * f);

    float n000 = hash(i + vec3(0, 0, 0));
    float n100 = hash(i + vec3(1, 0, 0));
    float n010 = hash(i + vec3(0, 1, 0));
    float n110 = hash(i + vec3(1, 1, 0));
    float n001 = hash(i + vec3(0, 0, 1));
    float n101 = hash(i + vec3(1, 0, 1));
    float n011 = hash(i + vec3(0, 1, 1));
    float n111 = hash(i + vec3(1, 1, 1));

    float nx00 = mix(n000, n100, f.x);
    float nx10 = mix(n010, n110, f.x);
    float nx01 = mix(n001, n101, f.x);
    float nx11 = mix(n011, n111, f.x);

    float nxy0 = mix(nx00, nx10, f.y);
    float nxy1 = mix(nx01, nx11, f.y);

    return mix(nxy0, nxy1, f.z);
}
float fbm3D(vec3 p) {
    float value = 0.0;
    float amplitude = 0.5;
    for (int i = 0; i < 5; i++) {
        value += amplitude * cnoise3D(p);
        p *= 2.01;
        amplitude *= 0.5;
    }
    return value;
}

