/*float noisemed(vec3 x) {
    vec3 p = floor(x);
    vec3 f = fract(x);
    f = f * f * (3.0 - 2.0 * f);

    float n = p.x + p.y * 157.0 + 113.0 * p.z;
   return mix(
        mix(mix(hash(vec3(n, 0.0, 0.0)), hash(vec3(n + 1.0, 0.0, 0.0)), f.x),
            mix(hash(vec3(n + 157.0, 0.0, 0.0)), hash(vec3(n + 158.0, 0.0, 0.0)), f.x), f.y),
        mix(mix(hash(vec3(n + 113.0, 0.0, 0.0)), hash(vec3(n + 114.0, 0.0, 0.0)), f.x),
            mix(hash(vec3(n + 270.0, 0.0, 0.0)), hash(vec3(n + 271.0, 0.0, 0.0)), f.x), f.y),
        f.z);
       // return Get3DNoise(x);
}

float fbmend(vec3 p) {
    float value = 0.0;
    float amplitude = 0.5;
    float frequency = 1.0;
    for(int i = 0; i < 3; i++) {
        value += amplitude * noise(p * frequency);
        amplitude *= 0.5;
        frequency *= 2.0;
    }
    
    return value;
}


float stormDensity(vec3 p, float t) {
    float r = length(p.xz);
    float angle = atan2(p.z, p.x);
    float height = p.y;
    if(r > 4.0 || height < -0.5 || height > 3.0) return 0.0;
    float spiralRate = 1.2 - r * 0.15; // Spiral gets looser as radius increases
    float spiral = angle - log(r + 0.1) * spiralRate + t * 0.3;

    float armCount = 3.5;
    float arms = sin(spiral * armCount) * 0.5 + 0.5;
    arms = pow(arms, 1.2);
    float spiralBands = smoothstep(0.25, 0.8, arms);
    spiralBands = pow(spiralBands, 0.7); 

    float eyeWall = smoothstep(0.25, 0.9, r) * smoothstep(3.8, 1.5, r);
    eyeWall = pow(eyeWall, 0.7);

    float eye = smoothstep(0.6, 0.1, r);


    float vortexAngle = spiral * 2.0;
    float vortexArms = sin(vortexAngle * 2.0) * 0.5 + 0.5;
    vortexArms = pow(vortexArms, 2.0);
    float innerSpiral = smoothstep(2.5, 0.5, r);
    float tightSpiral = sin((spiral + r * 0.3) * (3.0 + innerSpiral * 4.0)) * 0.5 + 0.5;
    tightSpiral = smoothstep(0.35, 0.65, tightSpiral);


    float density = eyeWall * spiralBands * (1.0 - eye * 0.97);
    density *= (0.5 + tightSpiral * 0.3 + vortexArms * 0.2);


    vec3 texCoord = p * 2.0;
   vec2 rotated = mat2(cos(r * 0.3), -sin(r * 0.3), 
                    sin(r * 0.3), cos(r * 0.3)) * p.xz * 2.0;
texCoord.x = rotated.x;
texCoord.z = rotated.y;

    texCoord += vec3(t * 0.1, 0.0, 0.0);
    
    float cloudTexture = fbmend(texCoord);
    cloudTexture = cloudTexture * 0.6 + 0.4; // Don't let texture remove too much density
    density *= cloudTexture;
    float heightFalloff = exp(-abs(height - 0.5) * 1.2);
    heightFalloff *= (0.65 + 0.35 * smoothstep(2.5, 0.3, r));
    density *= heightFalloff;
    density *= smoothstep(4.0, 2.8, r);
    float tendrils = sin(spiral * 7.0 - t * 0.6) * 0.5 + 0.5;
    tendrils = pow(tendrils, 1.5);
    tendrils *= smoothstep(3.8, 2.2, r) * smoothstep(1.3, 2.5, r);
    float trailingArms = sin(spiral * 12.0 + t * 0.4) * 0.5 + 0.5;
    trailingArms *= smoothstep(3.5, 2.0, r) * smoothstep(0.8, 2.0, r);
    trailingArms *= 0.15;
    
    density += tendrils * 0.25 + trailingArms;
    float turbulence = fbmend(p * 3.0 + vec3(cos(spiral) * 0.5, 0.0, sin(spiral) * 0.5));
    density *= (0.85 + turbulence * 0.15);

    return clamp(density * 0.9, 0.0, 1.0);
}

struct LightningBolt {
    vec3 pos;
    float intensity;
};
void getLightning(float t, out LightningBolt lights[2]) {
    float angle1 = t * 0.5;
    float flash1 = exp(-fract(t * 0.18) * 70.0);
    lights[0].pos = vec3(cos(angle1) * 2.0, 1.5, sin(angle1) * 2.0);
    lights[0].intensity = flash1 * 2.5;

    float angle2 = t * 0.6 + 2.0;
    float flash2 = exp(-fract(t * 0.13 + 0.5) * 60.0);
    lights[1].pos = vec3(cos(angle2) * 2.2, 1.8, sin(angle2) * 2.2);
    lights[1].intensity = flash2 * 2.0;
}


float phaseSimple(float cosTheta) {
    return 0.25 + 0.75 * (1.0 + cosTheta * 0.5);
}
vec3 volumetricLight(vec3 pos, vec3 viewDir, LightningBolt light, float cloudDensity,float u_time, out vec3 LDir) {
    if(light.intensity < 0.01) return vec3(0.0);

    vec3 toLight = light.pos - pos;
    float lightDist = length(toLight);
    vec3 lightDir = toLight / lightDist;
LDir = lightDir;

    float atten = 1.0 / (1.0 + lightDist * lightDist * 0.2);
    atten *= exp(-lightDist * 0.1);
    float shadow = 1.0;
  /*  for(int i = 0; i < LIGHT_SAMPLES; i++) {
        float fi = float(i) / float(LIGHT_SAMPLES);
        vec3 samplePos = pos + lightDir * lightDist * fi;
        shadow *= exp(-stormDensity(samplePos, u_time) * 0.35);
    }*/
/*

    float cosTheta = dot(viewDir, lightDir);
    float phase = phaseSimple(cosTheta);

    float directGlow = exp(-lightDist * 0.8) * light.intensity * 0.5;

    vec3 lightColor = vec3(0.7, 0.85, 1.0);
    vec3 scattered = lightColor * light.intensity * atten * shadow * phase * cloudDensity * 2.5;
    vec3 direct = lightColor * directGlow;

    return scattered + direct;
}
#define MARCH_STEPS 20
#define MARCH_SIZE 0.7
#define CLOUD_ABSORPTION 6.8
#define LIGHT_SAMPLES 1
#define CLOUD_CENTER_HEIGHT 35.0 
#define CLOUD_SCALE 25.0 

*/
