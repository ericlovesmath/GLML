open Core

let examples_dir = "../examples"

let test_examples () =
  let glml_files = Stdlib.Sys.readdir examples_dir in
  Array.iter glml_files ~f:(fun file ->
    let source = In_channel.read_all (Filename.concat examples_dir file) in
    Printf.printf "\n\n====== COMPILING EXAMPLE %s ======\n\n" file;
    Runner.test source)
;;

let%expect_test "compile examples" =
  test_examples ();
  [%expect
    {|
    ====== COMPILING EXAMPLE 2d_sdf_variants.glml ======

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    const vec3 blue = vec3(0.65, 0.85, 1.);
    const vec3 orange = vec3(0.9, 0.6, 0.3);
    uniform vec2 u_mouse;
    uniform vec2 u_resolution;
    vec2 get_uv_m(vec2 coord) {
        vec2 anf_14 = (2. * coord);
        vec2 top = (anf_14 - u_resolution);
        float anf_15 = u_resolution[0];
        float anf_16 = u_resolution[1];
        float bot = min(anf_15, anf_16);
        return (top / bot);
    }
    vec3 main_pure(vec2 coord_0) {
        vec2 p_1 = get_uv_m(coord_0);
        vec2 m = get_uv_m(u_mouse);
        float anf_2_6 = length(p_1);
        float anf_17_2 = (anf_2_6 - 0.3);
        vec2 anf_3_5 = abs(p_1);
        vec2 anf_4_5 = vec2(0.7, 0.1);
        vec2 d_8 = (anf_3_5 - anf_4_5);
        vec2 anf_5_5 = vec2(0., 0.);
        vec2 anf_6_5 = max(d_8, anf_5_5);
        float anf_7_5 = length(anf_6_5);
        float anf_8_5 = d_8[0];
        float anf_9_5 = d_8[1];
        float anf_10_5 = max(anf_8_5, anf_9_5);
        float anf_11_5 = min(anf_10_5, 0.);
        float anf_18_2 = (anf_7_5 + anf_11_5);
        float d_0 = min(anf_17_2, anf_18_2);
        bool anf_19 = (d_0 > 0.);
        vec3 col;
        if (anf_19) {
            col = orange;
        } else {
            col = blue;
        }
        float anf_20 = abs(d_0);
        float anf_21 = (-6. * anf_20);
        float anf_22 = exp(anf_21);
        float darken = (1. - anf_22);
        float anf_23 = (150. * d_0);
        float anf_24 = cos(anf_23);
        float anf_25 = (0.2 * anf_24);
        float rings = (0.8 + anf_25);
        vec3 anf_26 = (col * darken);
        vec3 col_0 = (anf_26 * rings);
        vec3 anf_27 = vec3(1., 1., 1.);
        float anf_28 = abs(d_0);
        float anf_29 = smoothstep(0., 0.01, anf_28);
        float anf_30 = (1. - anf_29);
        vec3 col_1 = mix(col_0, anf_27, anf_30);
        float anf_2_2 = length(m);
        float anf_17_1 = (anf_2_2 - 0.3);
        vec2 anf_3_1 = abs(m);
        vec2 anf_4_1 = vec2(0.7, 0.1);
        vec2 d_4 = (anf_3_1 - anf_4_1);
        vec2 anf_5_1 = vec2(0., 0.);
        vec2 anf_6_1 = max(d_4, anf_5_1);
        float anf_7_1 = length(anf_6_1);
        float anf_8_1 = d_4[0];
        float anf_9_1 = d_4[1];
        float anf_10_1 = max(anf_8_1, anf_9_1);
        float anf_11_1 = min(anf_10_1, 0.);
        float anf_18_1 = (anf_7_1 + anf_11_1);
        float anf_31 = min(anf_17_1, anf_18_1);
        float d_1 = abs(anf_31);
        vec2 anf_32 = (p_1 - m);
        float dm = length(anf_32);
        float anf_33 = (dm - d_1);
        float anf_34 = abs(anf_33);
        float anf_35 = (anf_34 - 0.0025);
        float anf_36 = (dm - 0.015);
        float d_2 = min(anf_35, anf_36);
        vec3 anf_37 = vec3(1., 1., 0.);
        float anf_38 = smoothstep(0., 0.005, d_2);
        float anf_39 = (1. - anf_38);
        vec3 col_2 = mix(col_1, anf_37, anf_39);
        return col_2;
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }


    ====== COMPILING EXAMPLE beaver.glml ======

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    const vec3 black = vec3(0.06, 0.04, 0.03);
    const vec3 brown = vec3(0.55, 0.34, 0.16);
    const vec3 brown_dk = vec3(0.38, 0.22, 0.08);
    const vec3 brown_lt = vec3(0.68, 0.46, 0.24);
    const vec3 cream = vec3(0.95, 0.89, 0.74);
    const vec3 cream_dk = vec3(0.78, 0.68, 0.5);
    const vec3 drk_brown = vec3(0.22, 0.11, 0.03);
    const vec3 grass_dk = vec3(0.34, 0.52, 0.22);
    const vec3 grass_lt = vec3(0.62, 0.78, 0.4);
    float ellipse(vec2 ab, vec2 p_0) {
        vec2 anf = (p_0 / ab);
        float anf_0 = length(anf);
        float anf_1 = (anf_0 - 1.);
        float anf_2 = ab[0];
        float anf_3 = ab[1];
        float anf_4 = min(anf_2, anf_3);
        return (anf_1 * anf_4);
    }
    float box(vec2 b, vec2 p_2) {
        vec2 anf_10 = abs(p_2);
        vec2 d = (anf_10 - b);
        vec2 anf_11 = vec2(0., 0.);
        vec2 anf_12 = max(d, anf_11);
        float anf_13 = length(anf_12);
        float anf_14 = d[0];
        float anf_15 = d[1];
        float anf_16 = max(anf_14, anf_15);
        float anf_17 = min(anf_16, 0.);
        return (anf_13 + anf_17);
    }
    vec3 paint_shaded(float d_2, vec3 shape_col_1, vec3 shadow_col, float shadow_depth, vec3 bg_1) {
        float anf_18 = smoothstep(-0.0025, 0.0025, d_2);
        vec3 base = mix(shape_col_1, bg_1, anf_18);
        float anf_19 = (-1. * shadow_depth);
        float shade = smoothstep(anf_19, 0., d_2);
        float anf_20 = (shade * 0.55);
        vec3 anf_21 = mix(shape_col_1, shadow_col, anf_20);
        float anf_22 = smoothstep(0.0025, -0.0025, d_2);
        return mix(base, anf_21, anf_22);
    }
    vec3 paint_w(float w, float d_0, vec3 shape_col, vec3 bg) {
        float anf_23 = (-1. * w);
        float anf_24 = smoothstep(anf_23, w, d_0);
        return mix(shape_col, bg, anf_24);
    }
    const vec3 pink = vec3(0.92, 0.62, 0.6);
    vec2 rot(float a, vec2 p_3) {
        float c = cos(a);
        float s = sin(a);
        float anf_25 = p_3[0];
        float anf_26 = (c * anf_25);
        float anf_27 = p_3[1];
        float anf_28 = (s * anf_27);
        float anf_29 = (anf_26 - anf_28);
        float anf_30 = p_3[0];
        float anf_31 = (s * anf_30);
        float anf_32 = p_3[1];
        float anf_33 = (c * anf_32);
        float anf_34 = (anf_31 + anf_33);
        return vec2(anf_29, anf_34);
    }
    float smin(float a_0, float b_0, float k) {
        float anf_35 = (a_0 - b_0);
        float anf_36 = abs(anf_35);
        float anf_37 = (k - anf_36);
        float anf_38 = max(anf_37, 0.);
        float h = (anf_38 / k);
        float anf_39 = min(a_0, b_0);
        float anf_40 = (h * h);
        float anf_41 = (anf_40 * k);
        float anf_42 = (anf_41 * 0.25);
        return (anf_39 - anf_42);
    }
    vec3 soft_tint(float d_4, vec3 col_0, float s_1, float f, vec3 bg_3) {
        float anf_43 = (-1. * f);
        float anf_44 = smoothstep(0., anf_43, d_4);
        float anf_45 = (s_1 * anf_44);
        return mix(bg_3, col_0, anf_45);
    }
    const vec3 tooth_yel = vec3(0.94, 0.88, 0.62);
    uniform vec2 u_resolution;
    const vec3 wht = vec3(1., 0.97, 0.93);
    vec3 main_pure(vec2 coord_0) {
        vec2 anf_48_0 = (2. * coord_0);
        vec2 top_0 = (anf_48_0 - u_resolution);
        float anf_49_0 = u_resolution[0];
        float anf_50_0 = u_resolution[1];
        float bot_0 = min(anf_49_0, anf_50_0);
        vec2 anf_51 = (top_0 / bot_0);
        vec2 p_4 = (anf_51 / 1.5);
        float anf_52 = p_4[1];
        float light = smoothstep(-0.45, 0.4, anf_52);
        float anf_53 = (light * 0.85);
        float anf_54 = (anf_53 + 0.1);
        vec3 body_col = mix(brown_dk, brown_lt, anf_54);
        float anf_55 = (light * 0.9);
        float anf_56 = (anf_55 + 0.05);
        vec3 belly_col = mix(cream_dk, cream, anf_56);
        float anf_57 = (light * 0.7);
        float anf_58 = (anf_57 + 0.2);
        vec3 ear_col = mix(brown_dk, brown, anf_58);
        float anf_59 = (light * 0.8);
        float anf_60 = (anf_59 + 0.15);
        vec3 muzzle_col = mix(cream_dk, cream, anf_60);
        float anf_61 = p_4[1];
        float anf_62 = smoothstep(-0.8, 0.8, anf_61);
        vec3 bg_4 = mix(grass_dk, grass_lt, anf_62);
        vec2 anf_63 = vec2(0.3, -0.3);
        vec2 anf_64 = (p_4 - anf_63);
        vec2 tp = rot(-0.35, anf_64);
        vec2 anf_65 = vec2(0.22, 0.085);
        float tail = ellipse(anf_65, tp);
        float anf_66 = (tail + 0.02);
        float anf_67 = (-1. * anf_66);
        float tail_rim = max(tail, anf_67);
        float anf_68 = tp[0];
        float anf_69 = tp[1];
        float anf_70 = (anf_68 + anf_69);
        float anf_71 = (anf_70 * 38.);
        float anf_72 = sin(anf_71);
        float g1 = abs(anf_72);
        float anf_73 = tp[0];
        float anf_74 = tp[1];
        float anf_75 = (anf_73 - anf_74);
        float anf_76 = (anf_75 * 38.);
        float anf_77 = sin(anf_76);
        float g2 = abs(anf_77);
        float anf_78 = min(g1, g2);
        float scale_pat = (anf_78 - 0.45);
        float anf_79 = (tail + 0.012);
        float anf_80 = (scale_pat * 0.012);
        float tail_scales = max(anf_79, anf_80);
        vec2 anf_81 = vec2(0.185, 0.135);
        vec2 anf_82 = vec2(0., -0.04);
        vec2 anf_83 = (p_4 - anf_82);
        float body_top = ellipse(anf_81, anf_83);
        vec2 anf_84 = vec2(0.26, 0.185);
        vec2 anf_85 = vec2(0., -0.24);
        vec2 anf_86 = (p_4 - anf_85);
        float body_bot = ellipse(anf_84, anf_86);
        float body = smin(body_top, body_bot, 0.12);
        vec2 anf_87 = vec2(0.2, 0.18);
        vec2 anf_88 = vec2(0., 0.18);
        vec2 anf_89 = (p_4 - anf_88);
        float head = ellipse(anf_87, anf_89);
        float torso = smin(body, head, 0.1);
        vec2 anf_90 = vec2(-0.128, 0.092);
        vec2 anf_91 = (p_4 - anf_90);
        float anf_5_13 = length(anf_91);
        float cheek_l = (anf_5_13 - 0.086);
        vec2 anf_92 = vec2(0.128, 0.092);
        vec2 anf_93 = (p_4 - anf_92);
        float anf_5_12 = length(anf_93);
        float cheek_r = (anf_5_12 - 0.086);
        float cheeks = min(cheek_l, cheek_r);
        float torso_with_cheeks = smin(torso, cheeks, 0.06);
        vec2 anf_94 = vec2(0.115, 0.082);
        vec2 anf_95 = vec2(0., 0.08);
        vec2 anf_96 = (p_4 - anf_95);
        float muzzle = ellipse(anf_94, anf_96);
        vec2 anf_97 = vec2(-0.118, 0.09);
        vec2 anf_98 = (p_4 - anf_97);
        float anf_5_11 = length(anf_98);
        float blush_l = (anf_5_11 - 0.03);
        vec2 anf_99 = vec2(0.118, 0.09);
        vec2 anf_100 = (p_4 - anf_99);
        float anf_5_10 = length(anf_100);
        float blush_r = (anf_5_10 - 0.03);
        vec2 anf_101 = vec2(0.125, 0.09);
        vec2 anf_102 = vec2(0., -0.1);
        vec2 anf_103 = (p_4 - anf_102);
        float belly_top = ellipse(anf_101, anf_103);
        vec2 anf_104 = vec2(0.16, 0.14);
        vec2 anf_105 = vec2(0., -0.24);
        vec2 anf_106 = (p_4 - anf_105);
        float belly_bot = ellipse(anf_104, anf_106);
        float belly = smin(belly_top, belly_bot, 0.12);
        vec2 anf_107 = vec2(-0.155, 0.318);
        vec2 anf_108 = (p_4 - anf_107);
        float anf_5_9 = length(anf_108);
        float ear_l = (anf_5_9 - 0.065);
        vec2 anf_109 = vec2(0.155, 0.318);
        vec2 anf_110 = (p_4 - anf_109);
        float anf_5_8 = length(anf_110);
        float ear_r = (anf_5_8 - 0.065);
        vec2 anf_111 = vec2(0.03, 0.036);
        vec2 anf_112 = vec2(-0.155, 0.31);
        vec2 anf_113 = (p_4 - anf_112);
        float ear_in_l = ellipse(anf_111, anf_113);
        vec2 anf_114 = vec2(0.03, 0.036);
        vec2 anf_115 = vec2(0.155, 0.31);
        vec2 anf_116 = (p_4 - anf_115);
        float ear_in_r = ellipse(anf_114, anf_116);
        vec2 anf_117 = vec2(0.045, 0.09);
        vec2 anf_118 = vec2(-0.13, -0.2);
        vec2 anf_119 = (p_4 - anf_118);
        vec2 anf_120 = rot(-0.7, anf_119);
        float arm_l = ellipse(anf_117, anf_120);
        vec2 anf_121 = vec2(0.045, 0.09);
        vec2 anf_122 = vec2(0.13, -0.2);
        vec2 anf_123 = (p_4 - anf_122);
        vec2 anf_124 = rot(0.7, anf_123);
        float arm_r = ellipse(anf_121, anf_124);
        vec2 anf_125 = vec2(-0.055, -0.255);
        vec2 anf_126 = (p_4 - anf_125);
        float anf_5_7 = length(anf_126);
        float paw_l = (anf_5_7 - 0.052);
        vec2 anf_127 = vec2(0.055, -0.255);
        vec2 anf_128 = (p_4 - anf_127);
        float anf_5_6 = length(anf_128);
        float paw_r = (anf_5_6 - 0.052);
        vec2 anf_129 = vec2(0.095, 0.048);
        vec2 anf_130 = vec2(-0.13, -0.42);
        vec2 anf_131 = (p_4 - anf_130);
        float foot_l = ellipse(anf_129, anf_131);
        vec2 anf_132 = vec2(0.095, 0.048);
        vec2 anf_133 = vec2(0.13, -0.42);
        vec2 anf_134 = (p_4 - anf_133);
        float foot_r = ellipse(anf_132, anf_134);
        vec2 anf_135 = vec2(0.018, 0.04);
        vec2 anf_136 = vec2(-0.022, 0.022);
        vec2 anf_137 = (p_4 - anf_136);
        float tooth_l = box(anf_135, anf_137);
        vec2 anf_138 = vec2(0.018, 0.04);
        vec2 anf_139 = vec2(0.022, 0.022);
        vec2 anf_140 = (p_4 - anf_139);
        float tooth_r = box(anf_138, anf_140);
        float teeth = min(tooth_l, tooth_r);
        vec2 anf_141 = vec2(0.004, 0.04);
        vec2 anf_142 = vec2(0., 0.022);
        vec2 anf_143 = (p_4 - anf_142);
        float groove = box(anf_141, anf_143);
        vec2 anf_144 = vec2(0.032, 0.024);
        vec2 anf_145 = vec2(0., 0.122);
        vec2 anf_146 = (p_4 - anf_145);
        float nose = ellipse(anf_144, anf_146);
        vec2 anf_147 = vec2(0.01, 0.007);
        vec2 anf_148 = vec2(-0.01, 0.128);
        vec2 anf_149 = (p_4 - anf_148);
        float nose_hi = ellipse(anf_147, anf_149);
        vec2 anf_150 = vec2(-0.085, 0.212);
        vec2 anf_151 = (p_4 - anf_150);
        float anf_5_5 = length(anf_151);
        float eye_l = (anf_5_5 - 0.034);
        vec2 anf_152 = vec2(0.085, 0.212);
        vec2 anf_153 = (p_4 - anf_152);
        float anf_5_4 = length(anf_153);
        float eye_r = (anf_5_4 - 0.034);
        vec2 anf_154 = vec2(-0.074, 0.224);
        vec2 anf_155 = (p_4 - anf_154);
        float anf_5_3 = length(anf_155);
        float hi_l = (anf_5_3 - 0.012);
        vec2 anf_156 = vec2(0.096, 0.224);
        vec2 anf_157 = (p_4 - anf_156);
        float anf_5_2 = length(anf_157);
        float hi_r = (anf_5_2 - 0.012);
        vec2 anf_158 = vec2(-0.094, 0.204);
        vec2 anf_159 = (p_4 - anf_158);
        float anf_5_1 = length(anf_159);
        float hi_l2 = (anf_5_1 - 0.005);
        vec2 anf_160 = vec2(0.076, 0.204);
        vec2 anf_161 = (p_4 - anf_160);
        float anf_5_0 = length(anf_161);
        float hi_r2 = (anf_5_0 - 0.005);
        vec2 anf_162 = vec2(0.03, 0.008);
        vec2 anf_163 = vec2(-0.085, 0.266);
        vec2 anf_164 = (p_4 - anf_163);
        vec2 anf_165 = rot(0.2, anf_164);
        float brow_l = ellipse(anf_162, anf_165);
        vec2 anf_166 = vec2(0.03, 0.008);
        vec2 anf_167 = vec2(0.085, 0.266);
        vec2 anf_168 = (p_4 - anf_167);
        vec2 anf_169 = rot(-0.2, anf_168);
        float brow_r = ellipse(anf_166, anf_169);
        vec2 anf_170 = vec2(-0.115, 0.085);
        vec2 anf_171 = (p_4 - anf_170);
        vec2 wp_l = rot(0.05, anf_171);
        vec2 anf_172 = vec2(0.115, 0.085);
        vec2 anf_173 = (p_4 - anf_172);
        vec2 wp_r = rot(-0.05, anf_173);
        vec2 anf_174 = vec2(0.04, 0.001);
        vec2 anf_175 = vec2(-0.04, 0.01);
        vec2 anf_176 = (wp_l - anf_175);
        float whisk_l1 = box(anf_174, anf_176);
        vec2 anf_177 = vec2(0.04, 0.001);
        vec2 anf_178 = vec2(-0.04, -0.01);
        vec2 anf_179 = (wp_l - anf_178);
        float whisk_l2 = box(anf_177, anf_179);
        vec2 anf_180 = vec2(0.04, 0.001);
        vec2 anf_181 = vec2(0.04, 0.01);
        vec2 anf_182 = (wp_r - anf_181);
        float whisk_r1 = box(anf_180, anf_182);
        vec2 anf_183 = vec2(0.04, 0.001);
        vec2 anf_184 = vec2(0.04, -0.01);
        vec2 anf_185 = (wp_r - anf_184);
        float whisk_r2 = box(anf_183, anf_185);
        float anf_186 = min(whisk_l1, whisk_l2);
        float anf_187 = min(whisk_r1, whisk_r2);
        float whiskers = min(anf_186, anf_187);
        vec2 anf_188 = vec2(0.165, 0.055);
        vec2 anf_189 = vec2(0., -0.02);
        vec2 anf_190 = (p_4 - anf_189);
        float neck_shadow = ellipse(anf_188, anf_190);
        vec2 anf_191 = vec2(0.11, 0.05);
        vec2 anf_192 = vec2(-0.025, 0.28);
        vec2 anf_193 = (p_4 - anf_192);
        float head_hilite = ellipse(anf_191, anf_193);
        vec2 anf_194 = vec2(0.13, 0.06);
        vec2 anf_195 = vec2(0., -0.3);
        vec2 anf_196 = (p_4 - anf_195);
        float belly_shadow = ellipse(anf_194, anf_196);
        vec2 anf_197 = vec2(0.4, 0.045);
        vec2 anf_198 = vec2(0.02, -0.46);
        vec2 anf_199 = (p_4 - anf_198);
        float shadow_d = ellipse(anf_197, anf_199);
        float shadow_falloff = smoothstep(0.06, -0.02, shadow_d);
        vec3 anf_200 = vec3(0.18, 0.28, 0.12);
        float anf_201 = (shadow_falloff * 0.5);
        vec3 bg_5 = mix(bg_4, anf_200, anf_201);
        float anf_202 = max(head_hilite, torso_with_cheeks);
        float anf_203 = min(blush_l, blush_r);
        float anf_204 = (-1. * belly);
        float anf_205 = max(torso_with_cheeks, anf_204);
        float anf_206 = max(neck_shadow, anf_205);
        float anf_207 = max(belly_shadow, belly);
        vec3 anf_208 = paint_w(0.0025, tail, brown_dk, bg_5);
        vec3 anf_209 = paint_w(0.0025, tail_rim, drk_brown, anf_208);
        vec3 anf_210 = paint_w(0.0025, tail_scales, drk_brown, anf_209);
        vec3 anf_211 = paint_w(0.0025, foot_l, drk_brown, anf_210);
        vec3 anf_212 = paint_w(0.0025, foot_r, drk_brown, anf_211);
        vec3 anf_213 = paint_shaded(torso_with_cheeks, body_col, brown_dk, 0.08, anf_212);
        vec3 anf_214 = paint_shaded(belly, belly_col, cream_dk, 0.05, anf_213);
        vec3 anf_215 = soft_tint(anf_207, brown_dk, 0.18, 0.08, anf_214);
        vec3 anf_216 = soft_tint(anf_206, brown_dk, 0.35, 0.05, anf_215);
        float anf_46_0 = smoothstep(0.003, -0.003, anf_203);
        float anf_47_0 = (0.35 * anf_46_0);
        vec3 anf_217 = mix(anf_216, pink, anf_47_0);
        vec3 anf_218 = paint_w(0.0025, arm_l, brown_dk, anf_217);
        vec3 anf_219 = paint_w(0.0025, arm_r, brown_dk, anf_218);
        vec3 anf_220 = paint_w(0.0025, paw_l, body_col, anf_219);
        vec3 anf_221 = paint_w(0.0025, paw_r, body_col, anf_220);
        vec3 anf_222 = paint_w(0.0025, ear_l, ear_col, anf_221);
        vec3 anf_223 = paint_w(0.0025, ear_r, ear_col, anf_222);
        vec3 anf_224 = paint_w(0.0025, ear_in_l, pink, anf_223);
        vec3 anf_225 = paint_w(0.0025, ear_in_r, pink, anf_224);
        vec3 anf_226 = paint_w(0.0025, muzzle, muzzle_col, anf_225);
        vec3 anf_227 = soft_tint(anf_202, wht, 0.18, 0.06, anf_226);
        vec3 anf_228 = paint_w(0.0025, brow_l, drk_brown, anf_227);
        vec3 anf_229 = paint_w(0.0025, brow_r, drk_brown, anf_228);
        vec3 anf_230 = paint_w(0.0025, teeth, tooth_yel, anf_229);
        vec3 anf_231 = paint_w(0.0025, groove, brown_dk, anf_230);
        vec3 anf_232 = paint_w(0.0025, nose, drk_brown, anf_231);
        vec3 anf_233 = paint_w(0.0025, nose_hi, brown_lt, anf_232);
        vec3 anf_234 = paint_w(0.0025, eye_l, black, anf_233);
        vec3 anf_235 = paint_w(0.0025, eye_r, black, anf_234);
        vec3 anf_236 = paint_w(0.003, whiskers, brown_dk, anf_235);
        vec3 anf_237 = paint_w(0.0025, hi_l, wht, anf_236);
        vec3 anf_238 = paint_w(0.0025, hi_r, wht, anf_237);
        vec3 anf_239 = paint_w(0.0025, hi_l2, wht, anf_238);
        return paint_w(0.0025, hi_r2, wht, anf_239);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }


    ====== COMPILING EXAMPLE checkerboard.glml ======

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform vec2 u_resolution;
    uniform float u_time;
    vec3 main_pure(vec2 coord_0) {
        vec2 anf_13 = (2. * coord_0);
        vec2 top_0 = (anf_13 - u_resolution);
        float anf_0_0 = u_resolution[0];
        float anf_1_0 = u_resolution[1];
        float bot_0 = min(anf_0_0, anf_1_0);
        vec2 uv = (top_0 / bot_0);
        vec2 anf_3 = (uv * 5.);
        float anf_4 = (2. * u_time);
        vec2 anf_5 = vec2(anf_4, 0.);
        vec2 anf_6 = (anf_3 + anf_5);
        vec2 c = floor(anf_6);
        float anf_7 = c[0];
        float anf_8 = c[1];
        float checker_sum = (anf_7 + anf_8);
        float anf_9 = (checker_sum / 2.);
        float anf_10 = floor(anf_9);
        float anf_11 = (anf_10 * 2.);
        float is_even = (checker_sum - anf_11);
        bool anf_12 = (is_even < 0.5);
        if (anf_12) {
            return vec3(0.2, 0.2, 0.2);
        } else {
            return vec3(0.8, 0.8, 0.8);
        }
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }


    ====== COMPILING EXAMPLE mandelbrot.glml ======

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct option {
        int tag;
        float Some_0;
    };
    option mandel_0(vec2 c, vec2 z, int i) {
        int _iter = 0;
        while (true) {
            bool _lim_cond = (_iter < 1000);
            if (_lim_cond) {
                bool anf = (i > 150);
                if (anf) {
                    return option(1, 0.);
                } else {
                    float anf_0 = length(z);
                    bool anf_1 = (anf_0 > 4.);
                    if (anf_1) {
                        float anf_2 = length(z);
                        float anf_3 = log2(anf_2);
                        float nu = log2(anf_3);
                        float anf_4 = float(i);
                        float anf_5 = (anf_4 - nu);
                        float anf_6 = (anf_5 / 150.);
                        return option(0, anf_6);
                    } else {
                        float anf_7 = z[0];
                        float anf_8 = z[0];
                        float anf_9 = (anf_7 * anf_8);
                        float anf_10 = z[1];
                        float anf_11 = z[1];
                        float anf_12 = (anf_10 * anf_11);
                        float zx = (anf_9 - anf_12);
                        float anf_13 = z[0];
                        float anf_14 = (2. * anf_13);
                        float anf_15 = z[1];
                        float zy = (anf_14 * anf_15);
                        vec2 anf_16 = vec2(zx, zy);
                        vec2 z_prime = (anf_16 + c);
                        int anf_17 = (i + 1);
                        int _iter_inc = (_iter + 1);
                        _iter = _iter_inc;
                        z = z_prime;
                        i = anf_17;
                        continue;
                    }
                }
            } else {
                option _tmp;
                return _tmp;
            }
        }
    }
    uniform vec2 u_resolution;
    uniform float u_time;
    vec3 main_pure(vec2 coord) {
        vec2 anf_19 = (2. * coord);
        vec2 top = (anf_19 - u_resolution);
        float anf_20 = u_resolution[0];
        float anf_21 = u_resolution[1];
        float bot = min(anf_20, anf_21);
        vec2 uv = (top / bot);
        float anf_22 = (u_time * 0.4);
        float anf_23 = sin(anf_22);
        float anf_24 = (anf_23 * 4.5);
        float anf_25 = (anf_24 + 3.5);
        float zoom = exp(anf_25);
        vec2 anf_26 = vec2(-0.7453, 0.1127);
        vec2 anf_27 = (uv / zoom);
        vec2 seahorse_valley = (anf_26 + anf_27);
        vec2 anf_18_0 = vec2(0., 0.);
        option anf_28 = mandel_0(seahorse_valley, anf_18_0, 0);
        int _lv_tag = anf_28.tag;
        switch (_lv_tag) {
            case 1: {
                return vec3(0., 0., 0.);
                break;
            }
            default: {
                float _lv_Some_0 = anf_28.Some_0;
                vec3 anf_29 = vec3(10., 20., 30.);
                vec3 anf_30 = (_lv_Some_0 * anf_29);
                vec3 anf_31 = (anf_30 + u_time);
                vec3 anf_32 = sin(anf_31);
                vec3 anf_33 = (anf_32 * 0.5);
                return (anf_33 + 0.5);
                break;
            }
        }
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }


    ====== COMPILING EXAMPLE mouse_circle.glml ======

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform vec2 u_mouse;
    uniform vec2 u_resolution;
    uniform float u_time;
    vec3 main_pure(vec2 coord_0) {
        vec2 anf_10 = (2. * coord_0);
        vec2 top_0 = (anf_10 - u_resolution);
        float anf_0_0 = u_resolution[0];
        float anf_1_0 = u_resolution[1];
        float bot_0 = min(anf_0_0, anf_1_0);
        vec2 uv = (top_0 / bot_0);
        vec2 anf_2 = (2. * u_mouse);
        vec2 anf_3 = (anf_2 - u_resolution);
        float anf_4 = u_resolution[1];
        vec2 mouseUV = (anf_3 / anf_4);
        float anf_5 = (u_time * 2.);
        float anf_6 = sin(anf_5);
        float anf_7 = (anf_6 * 0.1);
        float radius = (anf_7 + 0.15);
        float anf_8 = distance(uv, mouseUV);
        bool anf_9 = (anf_8 < radius);
        if (anf_9) {
            return vec3(0., 0., 0.5);
        } else {
            return vec3(0.5, 0.5, 1.);
        }
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }


    ====== COMPILING EXAMPLE planet.glml ======

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    const vec3 deepColor = vec3(0.02, 0.05, 0.2);
    float hash_0(vec3 p_1) {
        vec3 anf_3 = vec3(127.1, 311.7, 74.7);
        float d = dot(p_1, anf_3);
        float anf_4 = sin(d);
        float anf_5 = (anf_4 * 43758.5453);
        return fract(anf_5);
    }
    const vec3 landColor = vec3(0.15, 0.35, 0.1);
    const vec3 mountColor = vec3(0.4, 0.3, 0.2);
    float noise3d(vec3 p_0) {
        vec3 i = floor(p_0);
        vec3 f = fract(p_0);
        vec3 anf_6 = (f * f);
        vec3 anf_7 = (2. * f);
        vec3 anf_8 = (3. - anf_7);
        vec3 u = (anf_6 * anf_8);
        float a = hash_0(i);
        vec3 anf_9 = vec3(1., 0., 0.);
        vec3 anf_10 = (i + anf_9);
        float b = hash_0(anf_10);
        vec3 anf_11 = vec3(0., 1., 0.);
        vec3 anf_12 = (i + anf_11);
        float c_0 = hash_0(anf_12);
        vec3 anf_13 = vec3(1., 1., 0.);
        vec3 anf_14 = (i + anf_13);
        float d_0 = hash_0(anf_14);
        vec3 anf_15 = vec3(0., 0., 1.);
        vec3 anf_16 = (i + anf_15);
        float e = hash_0(anf_16);
        vec3 anf_17 = vec3(1., 0., 1.);
        vec3 anf_18 = (i + anf_17);
        float f_0 = hash_0(anf_18);
        vec3 anf_19 = vec3(0., 1., 1.);
        vec3 anf_20 = (i + anf_19);
        float g = hash_0(anf_20);
        vec3 anf_21 = vec3(1., 1., 1.);
        vec3 anf_22 = (i + anf_21);
        float h = hash_0(anf_22);
        float anf_23 = u[0];
        float ab = mix(a, b, anf_23);
        float anf_24 = u[0];
        float cd = mix(c_0, d_0, anf_24);
        float anf_25 = u[0];
        float ef = mix(e, f_0, anf_25);
        float anf_26 = u[0];
        float gh = mix(g, h, anf_26);
        float anf_27 = u[1];
        float abcd = mix(ab, cd, anf_27);
        float anf_28 = u[1];
        float efgh = mix(ef, gh, anf_28);
        float anf_29 = u[2];
        return mix(abcd, efgh, anf_29);
    }
    float fbm(vec3 p_2) {
        float anf_31 = noise3d(p_2);
        float anf_32 = (anf_31 * 0.5);
        vec3 anf_33 = (p_2 * 2.);
        float anf_34 = noise3d(anf_33);
        float anf_35 = (anf_34 * 0.25);
        float anf_36 = (anf_32 + anf_35);
        vec3 anf_37 = (p_2 * 4.);
        float anf_38 = noise3d(anf_37);
        float anf_39 = (anf_38 * 0.125);
        float anf_40 = (anf_36 + anf_39);
        vec3 anf_41 = (p_2 * 8.);
        float anf_42 = noise3d(anf_41);
        float anf_43 = (anf_42 * 0.0625);
        float anf_44 = (anf_40 + anf_43);
        vec3 anf_45 = (p_2 * 16.);
        float anf_46 = noise3d(anf_45);
        float anf_47 = (anf_46 * 0.03125);
        return (anf_44 + anf_47);
    }
    struct option {
        int tag;
        float Some_0;
    };
    vec2 rotate(vec2 p, float angle) {
        float s = sin(angle);
        float c = cos(angle);
        float anf_48 = p[0];
        float anf_49 = (anf_48 * c);
        float anf_50 = p[1];
        float anf_51 = (anf_50 * s);
        float anf_52 = (anf_49 - anf_51);
        float anf_53 = p[0];
        float anf_54 = (anf_53 * s);
        float anf_55 = p[1];
        float anf_56 = (anf_55 * c);
        float anf_57 = (anf_54 + anf_56);
        return vec2(anf_52, anf_57);
    }
    vec3 rotate_by_mouse_m_0(vec2 mouseUV, vec3 ray) {
        float anf_58 = mouseUV[1];
        float anf_59 = (-1. * anf_58);
        float rotX = (anf_59 * 1.5);
        float anf_60 = ray[1];
        float anf_61 = ray[2];
        vec2 anf_62 = vec2(anf_60, anf_61);
        vec2 ro_yz = rotate(anf_62, rotX);
        float anf_63 = mouseUV[0];
        float anf_64 = (-1. * anf_63);
        float rotY = (anf_64 * 1.5);
        float anf_65 = ray[0];
        float anf_66 = ro_yz[1];
        vec2 anf_67 = vec2(anf_65, anf_66);
        vec2 ro_xz = rotate(anf_67, rotY);
        float anf_68 = ro_xz[0];
        float anf_69 = ro_yz[0];
        float anf_70 = ro_xz[1];
        return vec3(anf_68, anf_69, anf_70);
    }
    option march_0_0(vec3 rd, vec3 ro, float t, int steps) {
        int _iter = 0;
        while (true) {
            bool _lim_cond = (_iter < 1000);
            if (_lim_cond) {
                bool anf_87 = (steps > 120);
                if (anf_87) {
                    return option(1, 0.);
                } else {
                    vec3 anf_88 = (rd * t);
                    vec3 anf_89 = (ro + anf_88);
                    float len_7 = length(anf_89);
                    vec3 dir_8 = (anf_89 / len_7);
                    vec3 anf_71_7 = (dir_8 * 3.);
                    float anf_72_7 = fbm(anf_71_7);
                    float terrain_7 = (anf_72_7 * 0.4);
                    float anf_73_7 = (len_7 - 1.5);
                    float d_1 = (anf_73_7 - terrain_7);
                    bool anf_90 = (d_1 < 0.0005);
                    if (anf_90) {
                        return option(0, t);
                    } else {
                        bool anf_91 = (t > 50.);
                        if (anf_91) {
                            return option(1, 0.);
                        } else {
                            float anf_92 = (d_1 * 0.8);
                            float anf_93 = (t + anf_92);
                            int anf_94 = (steps + 1);
                            int _iter_inc = (_iter + 1);
                            _iter = _iter_inc;
                            t = anf_93;
                            steps = anf_94;
                            continue;
                        }
                    }
                }
            } else {
                option _tmp;
                return _tmp;
            }
        }
    }
    const vec3 snowColor = vec3(0.85, 0.85, 0.9);
    uniform vec2 u_mouse;
    uniform vec2 u_resolution;
    vec3 main_pure(vec2 coord) {
        float anf_95 = u_resolution[0];
        float anf_96 = u_resolution[1];
        float res_min = min(anf_95, anf_96);
        vec2 anf_97 = (coord * 2.);
        vec2 anf_98 = (anf_97 - u_resolution);
        vec2 uv = (anf_98 / res_min);
        vec2 anf_99 = (u_mouse * 2.);
        vec2 anf_100 = (anf_99 - u_resolution);
        vec2 mouseUV = (anf_100 / res_min);
        vec3 anf_101 = vec3(0., 0., -4.);
        vec3 ro_0 = rotate_by_mouse_m_0(mouseUV, anf_101);
        float anf_102 = uv[0];
        float anf_103 = uv[1];
        vec3 anf_104 = vec3(anf_102, anf_103, 1.5);
        vec3 anf_105 = normalize(anf_104);
        vec3 rd_0 = rotate_by_mouse_m_0(mouseUV, anf_105);
        option t_0 = march_0_0(rd_0, ro_0, 0., 0);
        int _lv_tag = t_0.tag;
        switch (_lv_tag) {
            case 1: {
                return vec3(0., 0., 0.);
                break;
            }
            default: {
                float _lv_Some_0 = t_0.Some_0;
                vec3 anf_106 = (rd_0 * _lv_Some_0);
                vec3 hitPos = (ro_0 + anf_106);
                vec3 e_x_0 = vec3(0.002, 0., 0.);
                vec3 e_y_0 = vec3(0., 0.002, 0.);
                vec3 e_z_0 = vec3(0., 0., 0.002);
                vec3 anf_74_0 = (hitPos + e_x_0);
                float len_13 = length(anf_74_0);
                vec3 dir_14 = (anf_74_0 / len_13);
                vec3 anf_71_13 = (dir_14 * 3.);
                float anf_72_13 = fbm(anf_71_13);
                float terrain_13 = (anf_72_13 * 0.4);
                float anf_73_13 = (len_13 - 1.5);
                float anf_75_0 = (anf_73_13 - terrain_13);
                vec3 anf_76_0 = (hitPos - e_x_0);
                float len_12 = length(anf_76_0);
                vec3 dir_13 = (anf_76_0 / len_12);
                vec3 anf_71_12 = (dir_13 * 3.);
                float anf_72_12 = fbm(anf_71_12);
                float terrain_12 = (anf_72_12 * 0.4);
                float anf_73_12 = (len_12 - 1.5);
                float anf_77_0 = (anf_73_12 - terrain_12);
                float dx_0 = (anf_75_0 - anf_77_0);
                vec3 anf_78_0 = (hitPos + e_y_0);
                float len_11 = length(anf_78_0);
                vec3 dir_12 = (anf_78_0 / len_11);
                vec3 anf_71_11 = (dir_12 * 3.);
                float anf_72_11 = fbm(anf_71_11);
                float terrain_11 = (anf_72_11 * 0.4);
                float anf_73_11 = (len_11 - 1.5);
                float anf_79_0 = (anf_73_11 - terrain_11);
                vec3 anf_80_0 = (hitPos - e_y_0);
                float len_10 = length(anf_80_0);
                vec3 dir_11 = (anf_80_0 / len_10);
                vec3 anf_71_10 = (dir_11 * 3.);
                float anf_72_10 = fbm(anf_71_10);
                float terrain_10 = (anf_72_10 * 0.4);
                float anf_73_10 = (len_10 - 1.5);
                float anf_81_0 = (anf_73_10 - terrain_10);
                float dy_0 = (anf_79_0 - anf_81_0);
                vec3 anf_82_0 = (hitPos + e_z_0);
                float len_9 = length(anf_82_0);
                vec3 dir_10 = (anf_82_0 / len_9);
                vec3 anf_71_9 = (dir_10 * 3.);
                float anf_72_9 = fbm(anf_71_9);
                float terrain_9 = (anf_72_9 * 0.4);
                float anf_73_9 = (len_9 - 1.5);
                float anf_83_0 = (anf_73_9 - terrain_9);
                vec3 anf_84_0 = (hitPos - e_z_0);
                float len_8 = length(anf_84_0);
                vec3 dir_9 = (anf_84_0 / len_8);
                vec3 anf_71_8 = (dir_9 * 3.);
                float anf_72_8 = fbm(anf_71_8);
                float terrain_8 = (anf_72_8 * 0.4);
                float anf_73_8 = (len_8 - 1.5);
                float anf_85_0 = (anf_73_8 - terrain_8);
                float dz_0 = (anf_83_0 - anf_85_0);
                vec3 anf_86_0 = vec3(dx_0, dy_0, dz_0);
                vec3 n = normalize(anf_86_0);
                vec3 anf_107 = vec3(1., 0.8, -0.5);
                vec3 lightDir = normalize(anf_107);
                float anf_108 = dot(n, lightDir);
                float diff = max(anf_108, 0.);
                float anf_109 = length(hitPos);
                vec3 dir_0 = (hitPos / anf_109);
                vec3 anf_110 = (dir_0 * 3.);
                float rawHeight = fbm(anf_110);
                float anf_111 = (rawHeight - 0.35);
                float anf_113 = (anf_111 / 0.65);
                float h_norm = clamp(anf_113, 0., 1.);
                bool anf_114 = (h_norm < 0.3);
                vec3 baseColor;
                if (anf_114) {
                    float anf_115 = (h_norm / 0.3);
                    baseColor = mix(deepColor, landColor, anf_115);
                } else {
                    bool anf_116 = (h_norm < 0.6);
                    if (anf_116) {
                        float anf_117 = (h_norm - 0.3);
                        float anf_118 = (anf_117 / 0.3);
                        baseColor = mix(landColor, mountColor, anf_118);
                    } else {
                        float anf_119 = (h_norm - 0.6);
                        float anf_120 = (anf_119 / 0.4);
                        baseColor = mix(mountColor, snowColor, anf_120);
                    }
                }
                vec3 anf_121 = (-1. * rd_0);
                float anf_122 = dot(n, anf_121);
                float anf_123 = max(anf_122, 0.);
                float fresnel = (1. - anf_123);
                float anf_124 = (fresnel * fresnel);
                float anf_125 = (anf_124 * fresnel);
                float rim = (anf_125 * 0.4);
                vec3 atmoColor = vec3(0.3, 0.5, 1.);
                float anf_126 = (diff * 0.9);
                float anf_127 = (anf_126 + 0.08);
                vec3 anf_128 = (baseColor * anf_127);
                vec3 anf_129 = (atmoColor * rim);
                return (anf_128 + anf_129);
                break;
            }
        }
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }


    ====== COMPILING EXAMPLE rainbow.glml ======

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform vec2 u_resolution;
    uniform float u_time;
    vec3 main_pure(vec2 coord_0) {
        vec2 anf_10 = (2. * coord_0);
        vec2 top_0 = (anf_10 - u_resolution);
        float anf_0_0 = u_resolution[0];
        float anf_1_0 = u_resolution[1];
        float bot_0 = min(anf_0_0, anf_1_0);
        vec2 uv = (top_0 / bot_0);
        float anf_2 = uv[0];
        float anf_3 = uv[1];
        float anf_4 = (anf_2 + anf_3);
        float anf_5 = (5. * anf_4);
        float wave = (anf_5 + u_time);
        vec3 anf_6 = vec3(0., 2., 4.);
        vec3 anf_7 = (wave + anf_6);
        vec3 anf_8 = sin(anf_7);
        vec3 anf_9 = (anf_8 * 0.3);
        return (anf_9 + 0.7);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }


    ====== COMPILING EXAMPLE raymarch.glml ======

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    struct option {
        int tag;
        float Some_0;
    };
    vec2 rotate(vec2 p, float angle) {
        float s = sin(angle);
        float c = cos(angle);
        float anf_5 = p[0];
        float anf_6 = (anf_5 * c);
        float anf_7 = p[1];
        float anf_8 = (anf_7 * s);
        float anf_9 = (anf_6 - anf_8);
        float anf_10 = p[0];
        float anf_11 = (anf_10 * s);
        float anf_12 = p[1];
        float anf_13 = (anf_12 * c);
        float anf_14 = (anf_11 + anf_13);
        return vec2(anf_9, anf_14);
    }
    float sdTorus(vec3 p_0, vec2 t_0) {
        float anf_23 = p_0[0];
        float anf_24 = p_0[2];
        vec2 anf_25 = vec2(anf_23, anf_24);
        float anf_26 = length(anf_25);
        float anf_27 = t_0[0];
        float anf_28 = (anf_26 - anf_27);
        float anf_29 = p_0[1];
        vec2 q = vec2(anf_28, anf_29);
        float anf_30 = length(q);
        float anf_31 = t_0[1];
        return (anf_30 - anf_31);
    }
    uniform vec2 u_mouse;
    uniform vec2 u_resolution;
    uniform float u_time;
    option march_0_0(vec3 rd, vec3 ro, float t_1, int steps) {
        int _iter = 0;
        while (true) {
            bool _lim_cond = (_iter < 1000);
            if (_lim_cond) {
                bool anf_48 = (steps > 80);
                if (anf_48) {
                    return option(1, 0.);
                } else {
                    vec3 anf_49 = (rd * t_1);
                    vec3 anf_50 = (ro + anf_49);
                    float angle_0_0 = (u_time * 2.);
                    float anf_32_0 = anf_50[0];
                    float anf_33_0 = anf_50[1];
                    vec2 anf_34_0 = vec2(anf_32_0, anf_33_0);
                    vec2 p_xy_0 = rotate(anf_34_0, angle_0_0);
                    float anf_35_0 = p_xy_0[0];
                    float anf_36_0 = p_xy_0[1];
                    float anf_37_0 = anf_50[2];
                    vec2 anf_40_0 = vec2(anf_36_0, anf_37_0);
                    vec2 p_yz_0 = rotate(anf_40_0, angle_0_0);
                    float anf_42_0 = p_yz_0[0];
                    float anf_43_0 = p_yz_0[1];
                    vec3 p_prime_0_0 = vec3(anf_35_0, anf_42_0, anf_43_0);
                    vec2 anf_44_0 = vec2(1., 0.3);
                    float anf_45_0 = sdTorus(p_prime_0_0, anf_44_0);
                    vec2 anf_46_0 = vec2(2., 0.5);
                    float anf_47_0 = sdTorus(anf_50, anf_46_0);
                    float anf_15_1 = (anf_47_0 - anf_45_0);
                    float anf_16_1 = (0.5 * anf_15_1);
                    float anf_17_1 = (anf_16_1 / 0.1);
                    float anf_18_1 = (0.5 + anf_17_1);
                    float h_1 = clamp(anf_18_1, 0., 1.);
                    float anf_19_1 = mix(anf_47_0, anf_45_0, h_1);
                    float anf_20_1 = (0.1 * h_1);
                    float anf_21_1 = (1. - h_1);
                    float anf_22_1 = (anf_20_1 * anf_21_1);
                    float d = (anf_19_1 - anf_22_1);
                    bool anf_51 = (d < 0.001);
                    if (anf_51) {
                        return option(0, t_1);
                    } else {
                        bool anf_52 = (t_1 > 100.);
                        if (anf_52) {
                            return option(1, 0.);
                        } else {
                            float anf_53 = (t_1 + d);
                            int anf_54 = (steps + 1);
                            int _iter_inc = (_iter + 1);
                            _iter = _iter_inc;
                            t_1 = anf_53;
                            steps = anf_54;
                            continue;
                        }
                    }
                }
            } else {
                option _tmp;
                return _tmp;
            }
        }
    }
    vec3 main_pure(vec2 coord) {
        float anf_55 = u_resolution[0];
        float anf_56 = u_resolution[1];
        float res_min = min(anf_55, anf_56);
        vec2 anf_57 = (coord * 2.);
        vec2 anf_58 = (anf_57 - u_resolution);
        vec2 uv = (anf_58 / res_min);
        vec2 anf_59 = (u_mouse * 2.);
        vec2 anf_60 = (anf_59 - u_resolution);
        vec2 mouseUV = (anf_60 / res_min);
        float anf_61 = uv[0];
        float anf_62 = uv[1];
        vec3 anf_63 = vec3(anf_61, anf_62, 1.);
        vec3 rd_init = normalize(anf_63);
        float anf_64 = mouseUV[1];
        float rotX = (-1. * anf_64);
        float anf_65 = mouseUV[0];
        float rotY = (-1. * anf_65);
        vec2 anf_68 = vec2(0., -6.);
        vec2 ro_yz = rotate(anf_68, rotX);
        float anf_69 = rd_init[1];
        float anf_70 = rd_init[2];
        vec2 anf_71 = vec2(anf_69, anf_70);
        vec2 rd_yz = rotate(anf_71, rotX);
        float anf_73 = ro_yz[0];
        float anf_74 = ro_yz[1];
        float anf_75 = rd_init[0];
        float anf_76 = rd_yz[0];
        float anf_77 = rd_yz[1];
        vec2 anf_80 = vec2(0., anf_74);
        vec2 ro_xz = rotate(anf_80, rotY);
        vec2 anf_83 = vec2(anf_75, anf_77);
        vec2 rd_xz = rotate(anf_83, rotY);
        float anf_84 = ro_xz[0];
        float anf_86 = ro_xz[1];
        vec3 ro_1 = vec3(anf_84, anf_73, anf_86);
        float anf_87 = rd_xz[0];
        float anf_89 = rd_xz[1];
        vec3 rd_1 = vec3(anf_87, anf_76, anf_89);
        option anf_90 = march_0_0(rd_1, ro_1, 0., 0);
        int _lv_tag = anf_90.tag;
        vec3 col;
        switch (_lv_tag) {
            case 1: {
                col = vec3(0.2, 0.2, 0.2);
                break;
            }
            default: {
                float _lv_Some_0 = anf_90.Some_0;
                float anf_91 = (_lv_Some_0 * 0.3);
                vec3 cfg_0 = vec3(0.3, 0.416, 0.557);
                vec3 anf_1_0 = (cfg_0 + anf_91);
                vec3 anf_2_0 = (anf_1_0 * 6.28318);
                vec3 anf_3_0 = cos(anf_2_0);
                vec3 anf_4_0 = (anf_3_0 * 0.5);
                col = (anf_4_0 + 0.5);
                break;
            }
        }
        vec2 anf_92 = (uv - mouseUV);
        float anf_93 = length(anf_92);
        float glow = (0.02 / anf_93);
        return (col + glow);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }


    ====== COMPILING EXAMPLE recursion.glml ======

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float gcd_m(float a, float b) {
        int _iter = 0;
        while (true) {
            bool _lim_cond = (_iter < 1000);
            if (_lim_cond) {
                bool anf = (a < 0.05);
                if (anf) {
                    return b;
                } else {
                    bool anf_0 = (b < 0.05);
                    if (anf_0) {
                        return a;
                    } else {
                        bool anf_1 = (a > b);
                        if (anf_1) {
                            float anf_2 = (a - b);
                            int _iter_inc_0 = (_iter + 1);
                            _iter = _iter_inc_0;
                            a = anf_2;
                            continue;
                        } else {
                            float anf_3 = (b - a);
                            int _iter_inc = (_iter + 1);
                            _iter = _iter_inc;
                            b = anf_3;
                            continue;
                        }
                    }
                }
            } else {
                return 0.;
            }
        }
    }
    uniform vec2 u_resolution;
    uniform float u_time;
    vec3 main_pure(vec2 coord_0) {
        float s_0 = sin(u_time);
        float c_0 = cos(u_time);
        float anf_4_0 = (-1. * s_0);
        vec2 anf_5_0 = vec2(c_0, anf_4_0);
        vec2 anf_6_0 = vec2(s_0, c_0);
        mat2 anf_10 = mat2(anf_5_0, anf_6_0);
        vec2 anf_7_0 = (2. * coord_0);
        vec2 top_0 = (anf_7_0 - u_resolution);
        float anf_8_0 = u_resolution[0];
        float anf_9_0 = u_resolution[1];
        float bot_0 = min(anf_8_0, anf_9_0);
        vec2 anf_11 = (top_0 / bot_0);
        vec2 uv = (anf_10 * anf_11);
        float anf_12 = (u_time * 2.);
        float anf_13 = sin(anf_12);
        vec2 anf_14 = (uv * anf_13);
        vec2 anf_15 = (anf_14 * 2.);
        vec2 anf_16 = abs(anf_15);
        float _lv_v0 = anf_16[0];
        float _lv_v1 = anf_16[1];
        float res = gcd_m(_lv_v0, _lv_v1);
        float anf_17 = (res * 0.5);
        float anf_18 = (1. - res);
        return vec3(res, anf_17, anf_18);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }


    ====== COMPILING EXAMPLE warped_noise.glml ======

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float smoothNoise(vec2 p) {
        vec2 i = floor(p);
        vec2 pf = (p - i);
        vec2 anf_8 = (pf * pf);
        vec2 anf_9 = (2. * pf);
        vec2 anf_10 = (3. - anf_9);
        vec2 inter = (anf_8 * anf_10);
        vec4 v4 = vec4(0., 1., 27., 28.);
        float anf_11 = i[0];
        vec4 anf_12 = (v4 + anf_11);
        float anf_13 = i[1];
        float anf_14 = (anf_13 * 27.);
        vec4 seed = (anf_12 + anf_14);
        vec4 anf_15 = mod(seed, 6.2831853);
        vec4 anf_16 = sin(anf_15);
        vec4 anf_17 = (anf_16 * 200000.);
        vec4 hash = fract(anf_17);
        float anf_18 = hash[0];
        float anf_19 = hash[1];
        vec2 col0 = vec2(anf_18, anf_19);
        float anf_20 = hash[2];
        float anf_21 = hash[3];
        vec2 col1 = vec2(anf_20, anf_21);
        float anf_22 = inter[1];
        float anf_23 = (1. - anf_22);
        vec2 anf_24 = (col0 * anf_23);
        float anf_25 = inter[1];
        vec2 anf_26 = (col1 * anf_25);
        vec2 res_v = (anf_24 + anf_26);
        float anf_27 = inter[0];
        float anf_28 = (1. - anf_27);
        float anf_29 = inter[0];
        vec2 anf_30 = vec2(anf_28, anf_29);
        return dot(res_v, anf_30);
    }
    float fractalNoise(vec2 p_0) {
        float anf_31 = smoothNoise(p_0);
        float anf_32 = (anf_31 * 0.5333);
        vec2 anf_33 = (p_0 * 2.);
        float anf_34 = smoothNoise(anf_33);
        float anf_35 = (anf_34 * 0.2667);
        float anf_36 = (anf_32 + anf_35);
        vec2 anf_37 = (p_0 * 4.);
        float anf_38 = smoothNoise(anf_37);
        float anf_39 = (anf_38 * 0.1333);
        float anf_40 = (anf_36 + anf_39);
        vec2 anf_41 = (p_0 * 8.);
        float anf_42 = smoothNoise(anf_41);
        float anf_43 = (anf_42 * 0.0667);
        return (anf_40 + anf_43);
    }
    uniform vec2 u_resolution;
    uniform float u_time;
    float warpedNoise(vec2 p_1) {
        float anf_44 = (-1. * u_time);
        vec2 anf_45 = vec2(u_time, anf_44);
        vec2 m = (anf_45 * 0.5);
        vec2 anf_46 = (p_1 + m);
        float x = fractalNoise(anf_46);
        float anf_47 = m[1];
        float anf_48 = m[0];
        vec2 anf_49 = vec2(anf_47, anf_48);
        vec2 anf_50 = (p_1 + anf_49);
        vec2 anf_51 = (anf_50 + x);
        float y = fractalNoise(anf_51);
        vec2 anf_52 = (p_1 - m);
        vec2 anf_53 = (anf_52 - x);
        vec2 anf_54 = (anf_53 + y);
        float z = fractalNoise(anf_54);
        vec2 anf_55 = vec2(x, y);
        vec2 anf_56 = vec2(y, z);
        vec2 anf_57 = (anf_55 + anf_56);
        vec2 anf_58 = vec2(z, x);
        vec2 warp = (anf_57 + anf_58);
        vec3 anf_59 = vec3(x, y, z);
        float anf_60 = length(anf_59);
        float mag = (anf_60 * 0.25);
        vec2 anf_61 = (p_1 + warp);
        vec2 anf_62 = (anf_61 + mag);
        return fractalNoise(anf_62);
    }
    vec3 main_pure(vec2 coord) {
        vec2 anf_63 = (u_resolution * 0.5);
        vec2 anf_64 = (coord - anf_63);
        float anf_65 = u_resolution[1];
        vec2 uv = (anf_64 / anf_65);
        vec2 anf_66 = (uv * 6.);
        float n = warpedNoise(anf_66);
        vec2 anf_67 = (uv * 6.);
        vec2 anf_68 = (anf_67 - 0.02);
        float n2 = warpedNoise(anf_68);
        float anf_69 = (n2 - n);
        float anf_70 = max(anf_69, 0.);
        float anf_71 = (anf_70 / 0.02);
        float bump = (anf_71 * 0.7071);
        float anf_72 = (n - n2);
        float anf_73 = max(anf_72, 0.);
        float anf_74 = (anf_73 / 0.02);
        float bump2 = (anf_74 * 0.7071);
        float anf_75 = (bump * bump);
        float anf_76 = pow(bump, 4.);
        float anf_77 = (anf_76 * 0.5);
        float b1 = (anf_75 + anf_77);
        float anf_78 = (bump2 * bump2);
        float anf_79 = pow(bump2, 4.);
        float anf_80 = (anf_79 * 0.5);
        float b2 = (anf_78 + anf_80);
        vec3 anf_81 = vec3(1., 0.7, 0.6);
        float anf_82 = (b1 + b2);
        float anf_83 = (anf_82 * 0.4);
        vec3 anf_84 = vec3(b1, anf_83, b2);
        vec3 anf_85 = (anf_81 * anf_84);
        vec3 anf_86 = (anf_85 * 0.3);
        vec3 base_col = (anf_86 + 0.5);
        float anf_87 = (n * n);
        vec3 col = (anf_87 * base_col);
        vec2 anf_88 = (uv - 0.65);
        float spot1_dist = length(anf_88);
        vec2 anf_89 = (uv + 0.5);
        float spot2_dist = length(anf_89);
        vec3 anf_91 = vec3(0.27999999999999997, 0.13999999999999999, 0.35);
        vec3 anf_92 = vec3(1., 0.5, 0.2);
        float anf_93 = (1. - spot1_dist);
        float anf_94 = smoothstep(0., 1., anf_93);
        vec3 anf_95 = (anf_92 * anf_94);
        vec3 anf_96 = vec3(0.2, 0.4, 1.);
        float anf_97 = (1. - spot2_dist);
        float anf_98 = smoothstep(0., 1., anf_97);
        vec3 anf_99 = (anf_96 * anf_98);
        vec3 anf_100 = (anf_95 + anf_99);
        vec3 anf_101 = (anf_100 * 5.);
        vec3 spot_logic = (anf_91 + anf_101);
        vec3 final_col = (col * spot_logic);
        vec3 anf_102 = max(final_col, 0.);
        return sqrt(anf_102);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;
