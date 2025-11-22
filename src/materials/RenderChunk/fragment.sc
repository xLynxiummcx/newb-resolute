$input v_color0, v_color1, v_fog, v_refl, v_texcoord0, v_lightmapUV, v_extra, v_position, v_wpos, v_ao, v_tree, v_godray, v_camdist, v_tocamPos, v_waterNormal, view, cameraDir, v_lava, v_positionRelative

#define ENABLE_MIE 0
#include <bgfx_shader.sh>
#include <newb/main.sh>
#include <resolute/includes.h>

SAMPLER2D_AUTOREG(s_MatTexture);
SAMPLER2D_AUTOREG(s_SeasonsTexture);
SAMPLER2D_AUTOREG(s_LightMapTexture);
SAMPLER2D_AUTOREG(s_SunTex);
SAMPLER2D_AUTOREG(s_SSRTex);
SAMPLER2D_AUTOREG(s_NoiseCausticsPuddles);
SAMPLER2D_AUTOREG(s_Ripples);
SAMPLER2D_AUTOREG(s_NoiseVoxel);
SAMPLER2D_AUTOREG(s_MoonTex);
SAMPLER2D_AUTOREG(s_BlueNoise);
SAMPLER2D_AUTOREG(s_WaveOne);
SAMPLER2D_AUTOREG(s_WaveTwo);
SAMPLER2D_AUTOREG(s_WaveThree);

uniform vec4 ViewPositionAndTime;
uniform vec4 FogColor;
uniform vec4 FogAndDistanceControl;
uniform vec4 GameSunDir;
uniform vec4 GameBiomeID;
uniform vec4 GameCameraPos;
uniform vec4 GameTimeOfDay;
uniform vec4 GameWeatherID;
uniform vec4 GameDimension;


void main() {
#include <newb/functions/material/renderchunkFrag.h>
}