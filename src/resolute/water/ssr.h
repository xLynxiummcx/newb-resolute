vec4 textureCube2x3(sampler2D atlas, vec3 dir) {
    vec3 n = normalize(dir);
    float ax = abs(n.x);
    float ay = abs(n.y);
    float az = abs(n.z);

    float m = 0.0;
    vec2 uv = vec2(0.0,0.0);
    int face = 0;

    if (ax >= ay && ax >= az) {
        m = ax;
        if (n.x > 0.0) { face = 1; uv = vec2( n.z, n.y); }  // +X Right
        else           { face = 4; uv = vec2( -n.z, n.y); }  // -X Left
    }
    else if (ay >= ax && ay >= az) {
        m = ay;
        if (n.y < 0.0) { face = 2; uv = vec2( n.x,  n.z); }  // +Y Up
        else           { face = 3; uv = vec2( -n.z, n.x); }  // -Y Down
    }
    else {
        m = az;
        if (n.z > 0.0) { face = 0; uv = vec2( -n.x, n.y); }  // +Z Back
        else           { face = 5; uv = vec2( n.x, n.y); }  // -Z Front
    }

    uv = uv / m * 0.5 + 0.5;

    float f = float(face);
    float col = f - 2.0 * floor(f * 0.5);
    float row = floor(f * 0.5);

    vec2 atlasUV = (uv + vec2(col, row)) / vec2(2.0, 3.0);

    return texture2D(atlas, atlasUV);
}
