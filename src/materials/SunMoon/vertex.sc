$input a_color0, a_position, a_texcoord0
#ifdef INSTANCING
$input i_data1, i_data2, i_data3
#endif
$output v_texcoord0

#include <bgfx_shader.sh>
// Extra
uniform vec4  RenderChunkFogAlpha;
uniform vec4  FogAndDistanceControl;
uniform vec4  ViewPositionAndTime;
uniform vec4  FogColor;
uniform vec4  CloudColor;
uniform vec4  DistanceControl;
uniform vec4  SunMoonColor;
uniform vec4  FogControl;
uniform vec4  OverlayColor;
uniform vec4  TileLightColor;
uniform vec4  UVAnimation;
uniform mat4  Bones[8];
uniform vec4  SubPixelOffset;
uniform vec4  UseAlphaRewrite;
uniform vec4  ChangeColor;
uniform vec4  ColorBased;
uniform vec4  HudOpacity;
uniform vec4  LightDiffuseColorAndIlluminance;
uniform vec4  LightWorldSpaceDirection;
uniform vec4  MatColor;
uniform vec4  MultiplicativeTintColor;
uniform vec4  TintedAlphaTestEnabled;
uniform vec4  ActorFPEpsilon;
uniform mat4  View; // Separate from u_view
void main() {
  #ifdef INSTANCING
    mat4 model = mtxFromCols(i_data1, i_data2, i_data3, vec4(0.0, 0.0, 0.0, 1.0));
  #else
    mat4 model = u_model[0];
  #endif
  vec4 worldPos = mul(model, vec4(a_position, 1.0));


  v_texcoord0 = a_texcoord0;

  gl_Position = mul(u_viewProj, vec4(worldPos.xyz, 1.0));
}
