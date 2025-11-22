vec3 sunTextureMovement(
    vec3 sunDir, // sun direction
    vec3 rayDir, // view direction
    out float mask, sampler2D s_SunTex // output mask
) {
    vec3 forward = sunDir;
    vec3 right = normalize(cross(vec3(0.0, 1.0, 0.0), forward));
    vec3 up = cross(forward, right);

    float vdotl = dot(rayDir, sunDir);
    vec2 uv = vec2(dot(rayDir, right), dot(rayDir, up));

    const float sunSize = NL_SUN_SIZE;
    uv = uv / sunSize * 0.5 + 0.5;

    mask = 0.0;
    vec3 sunColor = vec3_splat(0.0);

    sunColor = texture2D(s_SunTex, uv).rgb;
    float lum = dot(sunColor, vec3(0.299, 0.587, 0.114));
    float maskLum = smoothstep(0.0, 0.9, lum);
    float distMask = smoothstep(0.5, 0.48, length(uv - 0.5));

    mask = maskLum * distMask;
    if (vdotl <= 0.0) {
        sunColor = vec3_splat(0.0);
    }
    return sunColor * mask;
}

vec3 moonTextureMovement(
	vec3 sunDir,//sun direction
	vec3 rayDir,//view direction
	out float mask, sampler2D s_MoonTex//outut mask for blending
	){
    vec3 forward = sunDir;
    vec3 right = normalize(cross(vec3(0.0,1.0,0.0), forward));
    vec3 up = cross(forward, right);
    float vdotl = dot(rayDir,sunDir);
    // Project rayDir into sun plane
    vec2 uv = vec2(dot(rayDir, right), dot(rayDir, up));
    float sunSize = 0.4;
    uv = uv / sunSize * 0.5 + 0.5;

    mask = 0.0;
    vec3 sunColor = vec3_splat(0.0);
    float alpha = 0.0;


      sunColor = texture2D(s_MoonTex, uv).rgb;
      float dist = length(uv - 0.5);

//remove blackness from texture (shader editor;
// don't know if the same issue will occur in Minecraft)
float lum = dot(sunColor, vec3(0.299,0.587,0.114));
float maskLum = smoothstep(0.0, 0.9, lum);
float distMask = smoothstep(0.5, 0.48, length(uv - 0.5));
mask = maskLum * distMask;
if(vdotl <=0.0){
sunColor = vec3_splat(0.0);
}
return sunColor * mask;
}
