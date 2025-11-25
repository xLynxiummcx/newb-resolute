//RenderChunk/fragment.sc
#if defined(DEPTH_ONLY_OPAQUE) || defined(DEPTH_ONLY) || defined(INSTANCING)
    gl_FragColor = vec4_splat(1.0);
    return;
#endif

vec3 viewPos = normalize(-v_wpos);
vec3 viewDir = -viewPos;       // normalize(v_wpos) = -normalize(-v_wpos)
float time = ViewPositionAndTime.w;

vec2 lm = v_lightmapUV;
float lmTorchIn = lm.x;
float lmSkyIn   = lm.y;

float weather = GameWeatherID.x;
nl_environment env = nlDetectEnvironment(FogColor.rgb, FogAndDistanceControl.xyz);

float rain  = env.rainFactor;
float clear = 1.0 - smoothstep(0.0, 0.5, weather);
float snow  = smoothstep(1.5, 2.0, weather);

vec4 whatTime = timeofday(GameTimeOfDay.x);
float night = whatTime.x;
float dawn  = whatTime.y;
float dusk  = whatTime.z;
float day   = whatTime.w;

vec3 cameraPos = GameCameraPos.xyz;
vec3 sunDir = normalize(GameSunDir.xyz);

float nightWarp = night * (1.0 - dawn) * (1.0 - dusk);
sunDir = normalize(mix(sunDir, vec3(-0.5, 0.2, 0.0), nightWarp));

bool water = v_extra.b > 0.9;

float lmSunY = lmSkyIn * lmSkyIn;
float shadows = smoothstep(0.9, 0.8, lmSunY);

vec3 N = normalize(cross(dFdx(v_position), dFdy(v_position)));
mat3 tbn = getTBN(N, v_texcoord0, v_position);

float noNormalMap = v_extra.b;
#if defined(NORMAL_MAP)
N = normalize(mul(tbn, getNormal(s_MatTexture, v_texcoord0)));
#endif

vec3 dVx = dFdx(v_position);
vec3 dVy = dFdy(v_position);

vec2 dLm = vec2(
    length(vec2(dFdx(lmTorchIn), dFdy(lmTorchIn))),
    length(vec2(dFdx(lmSkyIn),   dFdy(lmSkyIn)))
);

float hasTorch = step(1e-6, dLm.x);
float hasSky   = step(1e-6, dLm.y);

vec3 rawTorch = dVx * dFdx(lmTorchIn) + dVy * dFdy(lmTorchIn);
vec3 rawSky   = dVx * dFdx(lmSkyIn)   + dVy * dFdy(lmSkyIn);

const vec3 fallbackSky   = vec3(0.0, 1.0, 0.0);
vec3 fallbackTorch = normalize(mul(tbn,vec3(0.0, 0.0, 1.0)));

vec3 LT = normalize(mix(fallbackTorch, rawTorch, hasTorch));
vec3 LS = normalize(mix(fallbackSky,   rawSky,   hasSky));

float torchShade = saturate(dot(LT, N) + 0.8);
float skyShade   = saturate(dot(LS, N) + 0.8);

lmTorchIn = lmTorchIn * (torchShade * 0.8 + 0.2);
lmSkyIn   = lmSkyIn   * (skyShade   * 0.4 + 0.6);

lmTorchIn = clamp(lmTorchIn, 1.0/32.0, 31.0/32.0);
lmSkyIn   = clamp(lmSkyIn,   1.0/32.0, 31.0/32.0);

lm = vec2(lmTorchIn, lmSkyIn);

vec4 diffuse = texture2D(s_MatTexture, v_texcoord0);
float alpha = diffuse.a;

#ifdef ALPHA_TEST
    if (alpha < 0.5) discard;
#endif

vec4 vertexcol = v_color1;

#if defined(SEASONS) && (defined(ALPHA_TEST) || defined(OPAQUE))
    diffuse.rgb *= mix(vec3_splat(1.0),
                       2.0 * texture2D(s_SeasonsTexture, vertexcol.xy).rgb,
                       vertexcol.y);
    diffuse.rgb *= vertexcol.aaa;
    float foliage = 1.0;
#else
    diffuse.rgb *= vertexcol.rgb;
    float foliage = 0.0;
#endif

vec3 albedo = diffuse.rgb;

vec3 sunColor;
vec3 scf;

vec3 zenithsky = mainSkyRender(viewDir, sunDir, day, night, rain, dusk, dawn, snow, sunColor, time);
vec3 ambientSky = mainSkyRender(N, sunDir, day, night, rain, dusk, dawn, snow, scf, time);
vec3 storeSky = zenithsky;

if (env.nether) {
    storeSky  = vec3(0.75, 0.25, 0.1);
    zenithsky = vec3(0.71, 0.23, 0.00) * 1.2;
    sunColor  = vec3(1.0, 0.45, 0.1); 
    sunDir    = normalize(vec3(0.3, 1.0, 0.3));
    ambientSky = vec3(0.71, 0.23, 0.00) * 1.2;

} else if (env.end) {
    zenithsky = vec3_splat(0.01);
    sunColor  = vec3_splat(1.0);
    storeSky  = vec3_splat(0.0);
    ambientSky = vec3_splat(0.2);
}


sunColor *= (1.0 - shadows) * lmSkyIn;

float NoL = saturate(dot(N, sunDir));
float VoL = saturate(dot(viewPos, sunDir));

float backL = VoL * 0.5 + 0.5;
float backscatter = backL * backL * backL;
float wrap = saturate((NoL + 0.2) * (1.0 / 1.4));

float sss = foliage * (backscatter * wrap);
vec3 subsurfaceColor = albedo * sunColor * sss;

float amb = mix(lmSkyIn * lmSkyIn * lmSkyIn,
                lmSkyIn * lmSkyIn,
                lmSkyIn);

vec3 ambient = ambientSky * (0.1 * amb);
vec3 blockLight = lightBlockCol(lm, time, vertexcol.g, (1.0 - lmSkyIn) > 0.3);


vec3 specular = vec3_splat(0.0);

#if defined(PBR_SPECULAR)
 specular = brdf_specular(sunDir, viewPos, N, 0.2, vec3_splat(0.02), sunColor);
#endif

vec3 diffuseLight = sunColor * NoL + ambient;

diffuse.rgb = albedo * (diffuseLight + blockLight + specular) + subsurfaceColor;

diffuse.rgb = mix(diffuse.rgb, storeSky, v_fog.a*0.35);

diffuse.rgb = env.nether ? 
    colorCorrectionInv(diffuse.rgb) :
    colorCorrection(diffuse.rgb, rain);

gl_FragColor = diffuse;