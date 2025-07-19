right = (1.0 + step(0.5, TexCoord.s) * -2.0) * u_QF_ViewAxis[1] * u_QF_MirrorSide;
up = (1.0 + step(0.5, TexCoord.t) * -2.0) * u_QF_ViewAxis[2];
forward = -1.0 * u_QF_ViewAxis[0];
Position.xyz = a_SpritePoint.xyz + (right + up) * a_SpritePoint.w;
Normal.xyz = forward;
TexCoord.st = vec2(step(0.5, TexCoord.s),step(0.5, TexCoord.t));