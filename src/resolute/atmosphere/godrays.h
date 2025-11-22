float nsamp(vec2 coord, sampler2D bluen2){
 return texture2D(bluen2, coord/300.2).r;
 }
float getGodRays(vec2 coord, sampler2D bluen2, float time)
{
 float dither;
  dither = texture2D(bluen2,mod(coord,256.0)).r;
 float v = smoothstep(0.5, 0.9, 1.5 * nsamp((time+(coord) * (dither * 1.5) ),bluen2));
return v * (1.0/length(coord) * 3.0);
}
