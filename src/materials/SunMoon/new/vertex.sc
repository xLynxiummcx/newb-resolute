$input a_position, a_texcoord0
$output v_texcoord0, mj_pl, mj_pos

#include <bgfx_shader.sh>

#ifndef INSTANCING
  #include <newb/config.h>
#endif

uniform vec4 FogAndDistanceControl;
uniform vec4 FogColor;
uniform vec4 ViewPositionAndTime;

void main() {
  v_texcoord0 = a_texcoord0;
  mj_pos = a_position;
  mj_pl = mul(u_modelViewProj,vec4(a_position, 1.0));
  
  
   float malam = pow(max(min(1.0-FogColor.r*1.5,1.0),0.0),1.2);
    vec4 size = vec4(18.0, 1.0, 18.0, 1.0);
    size.xz /= mix( 1.0, 8.0, malam );
    gl_Position = mu(u_modelViewProj,vec4(a_position, 1.0) * size);
    //gl_Position = WORLDVIEWPROJ * (POSITION * size);
}
