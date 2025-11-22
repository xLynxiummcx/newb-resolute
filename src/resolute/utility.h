float luminance601(vec3 color) {
    return color.r * 0.299 + color.g * 0.587 + color.b * 0.114;
}
float luminance(vec3 color) {
    return color.r * 0.299 + color.g * 0.587 + color.b * 0.114;
}



float Bayer2(vec2 a) {
    a = floor(a);
    return fract(a.x / 2. + a.y * a.y * .75);
}

// doksli dither: https://www.shadertoy.com/view/7sfXDn
#define Bayer4(a)   (Bayer2 (.5 *(a)) * .25 + Bayer2(a))
#define Bayer8(a)   (Bayer4 (.5 *(a)) * .25 + Bayer2(a))
#define Bayer16(a)  (Bayer8 (.5 *(a)) * .25 + Bayer2(a))
#define Bayer32(a)  (Bayer16(.5 *(a)) * .25 + Bayer2(a))
#define Bayer64(a)  (Bayer32(.5 *(a)) * .25 + Bayer2(a))

#define PI 3.14159265359
float pow2(float x) { return x * x; }
float pow1_5(float x) { return pow(x, 1.5); }
float clamp01(float x) { return clamp(x, 0.0, 1.0); }
float sqrt1(float x) { return sqrt(max(x, 0.0)); }
vec3 getWeather(float weather){
float clear = 1.0 - smoothstep(0.0, 0.5, weather);
float rain  = smoothstep(0.5, 1.5, weather);
float snow  = smoothstep(1.5, 2.0, weather);

return vec3(clear,rain,snow);
}
vec4 timeofday(float dayTime){
    float day = smoothstep(0.77,0.82,dayTime)+(smoothstep(0.23, 0.18, dayTime)); 
    float night = smoothstep(0.22,0.27,dayTime) * (1.0 - smoothstep(0.73, 0.78, dayTime));  
    float dawn = smoothstep(0.695, 0.745, dayTime) * (1.0 - smoothstep(0.78, 0.83, dayTime));
    float dusk = smoothstep(0.16, 0.21, dayTime) * (1.0 - smoothstep(0.255, 0.305, dayTime));
   
  return vec4(night, dawn, dusk, day);
}

float fresnelSchlick(vec3 viewDir, vec3 normal, float F0) {
    float cosTheta = clamp(dot(normalize(viewDir), normalize(normal)), 0.0, 1.0);
    return F0 + (1.0 - F0) * pow(1.0 - cosTheta, 3.0);
}
