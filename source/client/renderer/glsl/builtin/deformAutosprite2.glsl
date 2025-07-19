right = QF_LatLong2Norm(a_SpriteRightUpAxis.xy) * u_QF_MirrorSide;
up = QF_LatLong2Norm(a_SpriteRightUpAxis.zw);

// mid of quad to camera vector
dist = u_QF_ViewOrigin - u_QF_EntityOrigin - a_SpritePoint.xyz;

// filter any longest-axis-parts off the camera-direction
forward = normalize(dist - up * dot(dist, up));

// the right axis vector as it should be to face the camera
newright = cross(up, forward);

// rotate the quad vertex around the up axis vector
t = dot(right, Position.xyz - a_SpritePoint.xyz);
Position.xyz += t * (newright - right);
Normal.xyz = forward;