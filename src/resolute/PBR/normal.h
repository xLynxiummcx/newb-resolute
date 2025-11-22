
mat3 getTBN(vec3 n,vec2 fragTexCoord,vec3 fragWorldPos) {
  /*  vec3 T = vec3(abs(normal.y) + normal.z, 0.0, normal.x);
    vec3 B = vec3(0.0, -abs(normal).x - abs(normal).z, abs(normal).y);
    vec3 N = normal;
    return transpose(mat3(T, B, N));*/
    vec2 dUVdx = dFdx(fragTexCoord);
    vec2 dUVdy = dFdy(fragTexCoord);
     vec3 dPdx = dFdx(fragWorldPos);
    vec3 dPdy = dFdy(fragWorldPos);
   
    vec3 N = n;
    vec3 T = normalize(dPdx * dUVdy.y - dPdy * dUVdx.y);
    vec3 B = normalize(cross(N, T));
    mat3 TBN = mat3(T, B, N);
   return TBN;
}
#define RESOLUTION 256.0

vec3 getNormal(sampler2D TEXTURE_0, vec2 coord) {
    float offsets = 1.0 / float(RESOLUTION) / 64.0;
    
    // 3x3 Sobel sampling
    float tl = luminance601(texture2D(TEXTURE_0, coord + vec2(-offsets, -offsets)).rgb);
    float t  = luminance601(texture2D(TEXTURE_0, coord + vec2(0.0, -offsets)).rgb);
    float tr = luminance601(texture2D(TEXTURE_0, coord + vec2(offsets, -offsets)).rgb);
    float l  = luminance601(texture2D(TEXTURE_0, coord + vec2(-offsets, 0.0)).rgb);
    float r  = luminance601(texture2D(TEXTURE_0, coord + vec2(offsets, 0.0)).rgb);
    float bl = luminance601(texture2D(TEXTURE_0, coord + vec2(-offsets, offsets)).rgb);
    float b  = luminance601(texture2D(TEXTURE_0, coord + vec2(0.0, offsets)).rgb);
    float br = luminance601(texture2D(TEXTURE_0, coord + vec2(offsets, offsets)).rgb);
    
    float dx = (tr + 2.0*r + br) - (tl + 2.0*l + bl);
    float dy = (bl + 2.0*b + br) - (tl + 2.0*t + tr);
    
    vec2 gradient = vec2(dx, dy) * 0.75;
    
    float lenSq = dot(gradient, gradient);
    vec3 normal = vec3(gradient, sqrt(max(0.0, 1.0 - lenSq)));
    
    return normalize(normal);
}

float detectTexture(inout bool reflective, inout bool slightly,float alpha){
#if !defined(TRANSPARENT) && !defined(ALPHA_TEST)
//opacity - 0.97
bool detecttexture2D = alpha > 0.965 && alpha < 0.975;
//opacity - 0.99
bool detect = alpha > 0.985 && alpha < 0.995;

if(detecttexture2D){
reflective = true;
}
if(detect){
slightly = true;
}
#endif

return 0.0;
}
