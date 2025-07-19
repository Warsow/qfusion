right = (1.0 + TexCoord.s * -2.0) * u_QF_ViewAxis[1] * u_QF_MirrorSide;
up = (1.0 + TexCoord.t * -2.0) * u_QF_ViewAxis[2];
forward = -1.0 * u_QF_ViewAxis[0];
// prevent the particle from disappearing at large distances
t = dot(a_SpritePoint.xyz + u_QF_EntityOrigin - u_QF_ViewOrigin, u_QF_ViewAxis[0]);
t = 1.5 + step(20.0, t) * t * 0.006;
Position.xyz = a_SpritePoint.xyz + (right + up) * t * a_SpritePoint.w;
Normal.xyz = forward;