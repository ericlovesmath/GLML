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
    struct shape {
        int tag;
        float Circle_0;
        float Rect_0;
        float Rect_1;
    };
    float sdf_shape(shape s, vec2 p) {
        int _lv_tag = s.tag;
        switch (_lv_tag) {
            case 0: {
                float r = s.Circle_0;
                float anf = length(p);
                return (anf - r);
                break;
            }
            case 1: {
                float w = s.Rect_0;
                float h = s.Rect_1;
                vec2 anf_0 = abs(p);
                vec2 anf_1 = vec2(w, h);
                vec2 d = (anf_0 - anf_1);
                vec2 anf_2 = vec2(0., 0.);
                vec2 anf_3 = max(d, anf_2);
                float anf_4 = length(anf_3);
                float anf_5 = d[0];
                float anf_6 = d[1];
                float anf_7 = max(anf_5, anf_6);
                float anf_8 = min(anf_7, 0.);
                return (anf_4 + anf_8);
                break;
            }
            default: {
                return 1.;
                break;
            }
        }
    }
    struct DFn {
        int tag;
        shape lctor_0;
        shape lctor_0_0;
    };
    struct DFn_0 {
        int tag;
        DFn lctor_1_0;
        DFn lctor_1_1;
    };
    DFn_0 scene() {
        shape anf_9 = shape(0, 0.3, 0., 0.);
        shape _tmp;
        DFn circle = DFn(0, anf_9, _tmp);
        shape anf_10 = shape(1, 0., 0.7, 0.1);
        shape _tmp_0;
        DFn rect = DFn(1, _tmp_0, anf_10);
        return DFn_0(0, circle, rect);
    }
    float dapply(DFn dfn, vec2 da) {
        int _lv_tag_0 = dfn.tag;
        switch (_lv_tag_0) {
            case 0: {
                shape ca = dfn.lctor_0;
                return sdf_shape(ca, da);
                break;
            }
            default: {
                shape ca_0 = dfn.lctor_0_0;
                return sdf_shape(ca_0, da);
                break;
            }
        }
    }
    uniform vec2 u_mouse;
    uniform vec2 u_resolution;
    vec2 get_uv_m(vec2 coord) {
        vec2 anf_11 = (2. * coord);
        vec2 top = (anf_11 - u_resolution);
        float anf_12 = u_resolution[0];
        float anf_13 = u_resolution[1];
        float bot = min(anf_12, anf_13);
        return (top / bot);
    }
    float union_0(DFn f, DFn f_prime, vec2 p_0) {
        float anf_14 = dapply(f, p_0);
        float anf_15 = dapply(f_prime, p_0);
        return min(anf_14, anf_15);
    }
    float dapply_0(DFn_0 dfn_0, vec2 da_0) {
        DFn ca_1 = dfn_0.lctor_1_0;
        DFn ca_2 = dfn_0.lctor_1_1;
        return union_0(ca_1, ca_2, da_0);
    }
    vec3 main_pure(vec2 coord_0) {
        vec2 p_1 = get_uv_m(coord_0);
        vec2 m = get_uv_m(u_mouse);
        DFn_0 _lc = scene();
        float d_0 = dapply_0(_lc, p_1);
        bool anf_16 = (d_0 > 0.);
        vec3 col;
        if (anf_16) {
            col = orange;
        } else {
            col = blue;
        }
        float anf_17 = abs(d_0);
        float anf_18 = (-6. * anf_17);
        float anf_19 = exp(anf_18);
        float darken = (1. - anf_19);
        float anf_20 = (150. * d_0);
        float anf_21 = cos(anf_20);
        float anf_22 = (0.2 * anf_21);
        float rings = (0.8 + anf_22);
        vec3 anf_23 = (col * darken);
        vec3 col_0 = (anf_23 * rings);
        vec3 anf_24 = vec3(1., 1., 1.);
        float anf_25 = abs(d_0);
        float anf_26 = smoothstep(0., 0.01, anf_25);
        float anf_27 = (1. - anf_26);
        vec3 col_1 = mix(col_0, anf_24, anf_27);
        DFn_0 _lc_0 = scene();
        float anf_28 = dapply_0(_lc_0, m);
        float d_1 = abs(anf_28);
        vec2 anf_29 = (p_1 - m);
        float dm = length(anf_29);
        float anf_30 = (dm - d_1);
        float anf_31 = abs(anf_30);
        float anf_32 = (anf_31 - 0.0025);
        float anf_33 = (dm - 0.015);
        float d_2 = min(anf_32, anf_33);
        vec3 anf_34 = vec3(1., 1., 0.);
        float anf_35 = smoothstep(0., 0.005, d_2);
        float anf_36 = (1. - anf_35);
        vec3 col_2 = mix(col_1, anf_34, anf_36);
        return col_2;
    }
    uniform float u_time;
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }


    ====== COMPILING EXAMPLE beaver.glml ======

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec2 at(vec2 offset, vec2 p) {
        return (p - offset);
    }
    const vec3 bg_col = vec3(0.13, 0.48, 0.3);
    const vec3 bg_dark = vec3(0.09, 0.36, 0.22);
    const vec3 black = vec3(0.06, 0.04, 0.03);
    float box(vec2 b, vec2 p_2) {
        vec2 anf = abs(p_2);
        vec2 d = (anf - b);
        vec2 anf_0 = vec2(0., 0.);
        vec2 anf_1 = max(d, anf_0);
        float anf_2 = length(anf_1);
        float anf_3 = d[0];
        float anf_4 = d[1];
        float anf_5 = max(anf_3, anf_4);
        float anf_6 = min(anf_5, 0.);
        return (anf_2 + anf_6);
    }
    const vec3 brown = vec3(0.55, 0.34, 0.16);
    const vec3 brown_dk = vec3(0.38, 0.22, 0.08);
    const vec3 brown_lt = vec3(0.68, 0.46, 0.24);
    float circle(float r, vec2 p_1) {
        float anf_7 = length(p_1);
        return (anf_7 - r);
    }
    const vec3 cream = vec3(0.95, 0.89, 0.74);
    const vec3 cream_dk = vec3(0.78, 0.68, 0.5);
    const vec3 drk_brown = vec3(0.22, 0.11, 0.03);
    float ellipse(vec2 ab, vec2 p_0) {
        vec2 anf_8 = (p_0 / ab);
        float anf_9 = length(anf_8);
        return (anf_9 - 1.);
    }
    vec3 paint(float d_0, vec3 shape_col, vec3 bg) {
        float anf_10 = smoothstep(-0.005, 0.005, d_0);
        return mix(shape_col, bg, anf_10);
    }
    vec3 paint_shaded(float d_1, vec3 shape_col_0, vec3 shadow_col, float shadow_depth, vec3 bg_0) {
        float anf_11 = smoothstep(-0.005, 0.005, d_1);
        vec3 base = mix(shape_col_0, bg_0, anf_11);
        float anf_12 = (-1. * shadow_depth);
        float shade = smoothstep(anf_12, 0., d_1);
        float anf_13 = (shade * 0.55);
        vec3 anf_14 = mix(shape_col_0, shadow_col, anf_13);
        float anf_15 = smoothstep(0.005, -0.005, d_1);
        return mix(base, anf_14, anf_15);
    }
    const vec3 pink = vec3(0.92, 0.62, 0.6);
    vec2 rot(float a, vec2 p_3) {
        float c = cos(a);
        float s = sin(a);
        float anf_16 = p_3[0];
        float anf_17 = (c * anf_16);
        float anf_18 = p_3[1];
        float anf_19 = (s * anf_18);
        float anf_20 = (anf_17 - anf_19);
        float anf_21 = p_3[0];
        float anf_22 = (s * anf_21);
        float anf_23 = p_3[1];
        float anf_24 = (c * anf_23);
        float anf_25 = (anf_22 + anf_24);
        return vec2(anf_20, anf_25);
    }
    float smin(float a_0, float b_0, float k) {
        float anf_26 = (a_0 - b_0);
        float anf_27 = abs(anf_26);
        float anf_28 = (k - anf_27);
        float anf_29 = max(anf_28, 0.);
        float h = (anf_29 / k);
        float anf_30 = min(a_0, b_0);
        float anf_31 = (h * h);
        float anf_32 = (anf_31 * k);
        float anf_33 = (anf_32 * 0.25);
        return (anf_30 - anf_33);
    }
    const vec3 tooth_yel = vec3(0.92, 0.85, 0.6);
    uniform vec2 u_resolution;
    vec2 get_uv_m(vec2 coord) {
        vec2 anf_34 = (2. * coord);
        vec2 top = (anf_34 - u_resolution);
        float anf_35 = u_resolution[0];
        float anf_36 = u_resolution[1];
        float bot = min(anf_35, anf_36);
        return (top / bot);
    }
    const vec3 wht = vec3(1., 0.97, 0.93);
    vec3 main_pure(vec2 coord_0) {
        vec2 anf_37 = get_uv_m(coord_0);
        vec2 p_4 = (anf_37 / 1.5);
        vec2 anf_38 = vec2(0.28, -0.28);
        vec2 anf_39 = at(anf_38, p_4);
        vec2 tp = rot(-0.35, anf_39);
        vec2 anf_40 = vec2(0.22, 0.085);
        float tail = ellipse(anf_40, tp);
        float anf_41 = tp[0];
        float anf_42 = tp[1];
        float anf_43 = (anf_41 + anf_42);
        float anf_44 = (anf_43 * 38.);
        float anf_45 = sin(anf_44);
        float anf_46 = abs(anf_45);
        float tsx = (anf_46 - 0.55);
        float anf_47 = tp[0];
        float anf_48 = tp[1];
        float anf_49 = (anf_47 - anf_48);
        float anf_50 = (anf_49 * 38.);
        float anf_51 = sin(anf_50);
        float anf_52 = abs(anf_51);
        float tsy = (anf_52 - 0.55);
        float anf_53 = (tail + 0.01);
        float anf_54 = min(tsx, tsy);
        float anf_55 = (anf_54 * 0.015);
        float tail_scales = max(anf_53, anf_55);
        float anf_56 = (tail + 0.018);
        float anf_57 = (-1. * anf_56);
        float tail_rim = max(tail, anf_57);
        vec2 anf_58 = vec2(0.25, 0.26);
        vec2 anf_59 = vec2(0., -0.16);
        vec2 anf_60 = at(anf_59, p_4);
        float body = ellipse(anf_58, anf_60);
        vec2 anf_61 = vec2(0.22, 0.2);
        vec2 anf_62 = vec2(0., 0.2);
        vec2 anf_63 = at(anf_62, p_4);
        float head = ellipse(anf_61, anf_63);
        float torso = smin(body, head, 0.1);
        vec2 anf_64 = vec2(-0.14, 0.11);
        vec2 anf_65 = at(anf_64, p_4);
        float cheek_l = circle(0.1, anf_65);
        vec2 anf_66 = vec2(0.14, 0.11);
        vec2 anf_67 = at(anf_66, p_4);
        float cheek_r = circle(0.1, anf_67);
        float cheeks = min(cheek_l, cheek_r);
        float torso_with_cheeks = smin(torso, cheeks, 0.06);
        vec2 anf_68 = vec2(0.13, 0.095);
        vec2 anf_69 = vec2(0., 0.09);
        vec2 anf_70 = at(anf_69, p_4);
        float muzzle = ellipse(anf_68, anf_70);
        vec2 anf_71 = vec2(0.15, 0.17);
        vec2 anf_72 = vec2(0., -0.2);
        vec2 anf_73 = at(anf_72, p_4);
        float belly = ellipse(anf_71, anf_73);
        vec2 anf_74 = vec2(-0.175, 0.355);
        vec2 anf_75 = at(anf_74, p_4);
        float ear_l = circle(0.075, anf_75);
        vec2 anf_76 = vec2(0.175, 0.355);
        vec2 anf_77 = at(anf_76, p_4);
        float ear_r = circle(0.075, anf_77);
        vec2 anf_78 = vec2(0.035, 0.042);
        vec2 anf_79 = vec2(-0.175, 0.345);
        vec2 anf_80 = at(anf_79, p_4);
        float ear_in_l = ellipse(anf_78, anf_80);
        vec2 anf_81 = vec2(0.035, 0.042);
        vec2 anf_82 = vec2(0.175, 0.345);
        vec2 anf_83 = at(anf_82, p_4);
        float ear_in_r = ellipse(anf_81, anf_83);
        vec2 anf_84 = vec2(0.065, 0.09);
        vec2 anf_85 = vec2(-0.23, -0.09);
        vec2 anf_86 = at(anf_85, p_4);
        float arm_l = ellipse(anf_84, anf_86);
        vec2 anf_87 = vec2(0.065, 0.09);
        vec2 anf_88 = vec2(0.23, -0.09);
        vec2 anf_89 = at(anf_88, p_4);
        float arm_r = ellipse(anf_87, anf_89);
        vec2 anf_90 = vec2(-0.28, -0.19);
        vec2 anf_91 = at(anf_90, p_4);
        float paw_l = circle(0.055, anf_91);
        vec2 anf_92 = vec2(0.28, -0.19);
        vec2 anf_93 = at(anf_92, p_4);
        float paw_r = circle(0.055, anf_93);
        vec2 anf_94 = vec2(0.095, 0.048);
        vec2 anf_95 = vec2(-0.13, -0.42);
        vec2 anf_96 = at(anf_95, p_4);
        float foot_l = ellipse(anf_94, anf_96);
        vec2 anf_97 = vec2(0.095, 0.048);
        vec2 anf_98 = vec2(0.13, -0.42);
        vec2 anf_99 = at(anf_98, p_4);
        float foot_r = ellipse(anf_97, anf_99);
        vec2 anf_100 = vec2(0.022, 0.05);
        vec2 anf_101 = vec2(-0.028, 0.035);
        vec2 anf_102 = at(anf_101, p_4);
        float tooth_l = box(anf_100, anf_102);
        vec2 anf_103 = vec2(0.022, 0.05);
        vec2 anf_104 = vec2(0.028, 0.035);
        vec2 anf_105 = at(anf_104, p_4);
        float tooth_r = box(anf_103, anf_105);
        float teeth = min(tooth_l, tooth_r);
        vec2 anf_106 = vec2(0.005, 0.05);
        vec2 anf_107 = vec2(0., 0.035);
        vec2 anf_108 = at(anf_107, p_4);
        float groove = box(anf_106, anf_108);
        vec2 anf_109 = vec2(0.038, 0.028);
        vec2 anf_110 = vec2(0., 0.135);
        vec2 anf_111 = at(anf_110, p_4);
        float nose = ellipse(anf_109, anf_111);
        vec2 anf_112 = vec2(0.012, 0.008);
        vec2 anf_113 = vec2(-0.012, 0.142);
        vec2 anf_114 = at(anf_113, p_4);
        float nose_hi = ellipse(anf_112, anf_114);
        vec2 anf_115 = vec2(-0.095, 0.255);
        vec2 anf_116 = at(anf_115, p_4);
        float eye_l = circle(0.04, anf_116);
        vec2 anf_117 = vec2(0.095, 0.255);
        vec2 anf_118 = at(anf_117, p_4);
        float eye_r = circle(0.04, anf_118);
        vec2 anf_119 = vec2(-0.082, 0.27);
        vec2 anf_120 = at(anf_119, p_4);
        float hi_l = circle(0.014, anf_120);
        vec2 anf_121 = vec2(0.108, 0.27);
        vec2 anf_122 = at(anf_121, p_4);
        float hi_r = circle(0.014, anf_122);
        vec2 anf_123 = vec2(-0.105, 0.245);
        vec2 anf_124 = at(anf_123, p_4);
        float hi_l2 = circle(0.006, anf_124);
        vec2 anf_125 = vec2(0.085, 0.245);
        vec2 anf_126 = at(anf_125, p_4);
        float hi_r2 = circle(0.006, anf_126);
        vec2 anf_127 = vec2(0.035, 0.01);
        vec2 anf_128 = vec2(-0.095, 0.315);
        vec2 anf_129 = at(anf_128, p_4);
        vec2 anf_130 = rot(0.15, anf_129);
        float brow_l = ellipse(anf_127, anf_130);
        vec2 anf_131 = vec2(0.035, 0.01);
        vec2 anf_132 = vec2(0.095, 0.315);
        vec2 anf_133 = at(anf_132, p_4);
        vec2 anf_134 = rot(-0.15, anf_133);
        float brow_r = ellipse(anf_131, anf_134);
        float anf_135 = length(p_4);
        float vig = smoothstep(0.3, 1.1, anf_135);
        vec3 col = mix(bg_col, bg_dark, vig);
        vec2 anf_136 = vec2(0.38, 0.055);
        vec2 anf_137 = vec2(0.02, -0.45);
        vec2 anf_138 = at(anf_137, p_4);
        float shadow_d = ellipse(anf_136, anf_138);
        float shadow_falloff = smoothstep(0.08, -0.02, shadow_d);
        vec3 anf_139 = vec3(0.06, 0.28, 0.16);
        float anf_140 = (shadow_falloff * 0.55);
        vec3 col_0 = mix(col, anf_139, anf_140);
        vec3 anf_141 = paint(tail, brown_dk, col_0);
        vec3 anf_142 = paint(tail_rim, drk_brown, anf_141);
        vec3 anf_143 = paint(tail_scales, drk_brown, anf_142);
        vec3 anf_144 = paint(foot_l, drk_brown, anf_143);
        vec3 anf_145 = paint(foot_r, drk_brown, anf_144);
        vec3 anf_146 = paint(arm_l, brown_dk, anf_145);
        vec3 anf_147 = paint(arm_r, brown_dk, anf_146);
        vec3 anf_148 = paint_shaded(torso_with_cheeks, brown, brown_dk, 0.08, anf_147);
        vec3 anf_149 = paint_shaded(belly, cream, cream_dk, 0.05, anf_148);
        vec3 anf_150 = paint(paw_l, cream, anf_149);
        vec3 anf_151 = paint(paw_r, cream, anf_150);
        vec3 anf_152 = paint(ear_l, brown, anf_151);
        vec3 anf_153 = paint(ear_r, brown, anf_152);
        vec3 anf_154 = paint(ear_in_l, pink, anf_153);
        vec3 anf_155 = paint(ear_in_r, pink, anf_154);
        vec3 anf_156 = paint(muzzle, cream, anf_155);
        vec3 anf_157 = paint(brow_l, drk_brown, anf_156);
        vec3 anf_158 = paint(brow_r, drk_brown, anf_157);
        vec3 anf_159 = paint(teeth, tooth_yel, anf_158);
        vec3 anf_160 = paint(groove, brown_dk, anf_159);
        vec3 anf_161 = paint(nose, drk_brown, anf_160);
        vec3 anf_162 = paint(nose_hi, brown_lt, anf_161);
        vec3 anf_163 = paint(eye_l, black, anf_162);
        vec3 anf_164 = paint(eye_r, black, anf_163);
        vec3 anf_165 = paint(hi_l, wht, anf_164);
        vec3 anf_166 = paint(hi_r, wht, anf_165);
        vec3 anf_167 = paint(hi_l2, wht, anf_166);
        return paint(hi_r2, wht, anf_167);
    }
    const vec3 pink_dk = vec3(0.75, 0.42, 0.42);
    float smax(float a_1, float b_1, float k_0) {
        float anf_168 = (a_1 - b_1);
        float anf_169 = abs(anf_168);
        float anf_170 = (k_0 - anf_169);
        float anf_171 = max(anf_170, 0.);
        float h_0 = (anf_171 / k_0);
        float anf_172 = max(a_1, b_1);
        float anf_173 = (h_0 * h_0);
        float anf_174 = (anf_173 * k_0);
        float anf_175 = (anf_174 * 0.25);
        return (anf_172 + anf_175);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }


    ====== COMPILING EXAMPLE checkerboard.glml ======

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    const int size = 5;
    uniform vec2 u_resolution;
    vec2 get_uv_m(vec2 coord) {
        vec2 anf = (2. * coord);
        vec2 top = (anf - u_resolution);
        float anf_0 = u_resolution[0];
        float anf_1 = u_resolution[1];
        float bot = min(anf_0, anf_1);
        return (top / bot);
    }
    uniform float u_time;
    vec3 main_pure(vec2 coord_0) {
        vec2 uv = get_uv_m(coord_0);
        float anf_2 = float(size);
        vec2 anf_3 = (uv * anf_2);
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
        while ((_iter < 1000)) {
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
                    c = c;
                    z = z_prime;
                    i = anf_17;
                    int _iter_inc = (_iter + 1);
                    _iter = _iter_inc;
                    continue;
                }
            }
        }
        option _tmp;
        return _tmp;
    }
    option mandelbrot_m(vec2 c) {
        vec2 anf_18 = vec2(0., 0.);
        return mandel_0(c, anf_18, 0);
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
        option anf_28 = mandelbrot_m(seahorse_valley);
        int _lv_tag = anf_28.tag;
        switch (_lv_tag) {
            case 1: {
                return vec3(0., 0., 0.);
                break;
            }
            default: {
                float n = anf_28.Some_0;
                vec3 anf_29 = vec3(10., 20., 30.);
                vec3 anf_30 = (n * anf_29);
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
    vec2 get_uv_m(vec2 coord) {
        vec2 anf = (2. * coord);
        vec2 top = (anf - u_resolution);
        float anf_0 = u_resolution[0];
        float anf_1 = u_resolution[1];
        float bot = min(anf_0, anf_1);
        return (top / bot);
    }
    uniform float u_time;
    vec3 main_pure(vec2 coord_0) {
        vec2 uv = get_uv_m(coord_0);
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
        vec3 anf = vec3(127.1, 311.7, 74.7);
        float d = dot(p_1, anf);
        float anf_0 = sin(d);
        float anf_1 = (anf_0 * 43758.5453);
        return fract(anf_1);
    }
    const vec3 landColor = vec3(0.15, 0.35, 0.1);
    const vec3 mountColor = vec3(0.4, 0.3, 0.2);
    float noise3d(vec3 p_0) {
        vec3 i = floor(p_0);
        vec3 f = fract(p_0);
        vec3 anf_2 = (f * f);
        vec3 anf_3 = (2. * f);
        vec3 anf_4 = (3. - anf_3);
        vec3 u = (anf_2 * anf_4);
        float a = hash_0(i);
        vec3 anf_5 = vec3(1., 0., 0.);
        vec3 anf_6 = (i + anf_5);
        float b = hash_0(anf_6);
        vec3 anf_7 = vec3(0., 1., 0.);
        vec3 anf_8 = (i + anf_7);
        float c_0 = hash_0(anf_8);
        vec3 anf_9 = vec3(1., 1., 0.);
        vec3 anf_10 = (i + anf_9);
        float d_0 = hash_0(anf_10);
        vec3 anf_11 = vec3(0., 0., 1.);
        vec3 anf_12 = (i + anf_11);
        float e = hash_0(anf_12);
        vec3 anf_13 = vec3(1., 0., 1.);
        vec3 anf_14 = (i + anf_13);
        float f_0 = hash_0(anf_14);
        vec3 anf_15 = vec3(0., 1., 1.);
        vec3 anf_16 = (i + anf_15);
        float g = hash_0(anf_16);
        vec3 anf_17 = vec3(1., 1., 1.);
        vec3 anf_18 = (i + anf_17);
        float h = hash_0(anf_18);
        float anf_19 = u[0];
        float ab = mix(a, b, anf_19);
        float anf_20 = u[0];
        float cd = mix(c_0, d_0, anf_20);
        float anf_21 = u[0];
        float ef = mix(e, f_0, anf_21);
        float anf_22 = u[0];
        float gh = mix(g, h, anf_22);
        float anf_23 = u[1];
        float abcd = mix(ab, cd, anf_23);
        float anf_24 = u[1];
        float efgh = mix(ef, gh, anf_24);
        float anf_25 = u[2];
        return mix(abcd, efgh, anf_25);
    }
    float fbm(vec3 p_2) {
        vec3 anf_26 = (p_2 * 1.);
        float anf_27 = noise3d(anf_26);
        float anf_28 = (anf_27 * 0.5);
        vec3 anf_29 = (p_2 * 2.);
        float anf_30 = noise3d(anf_29);
        float anf_31 = (anf_30 * 0.25);
        float anf_32 = (anf_28 + anf_31);
        vec3 anf_33 = (p_2 * 4.);
        float anf_34 = noise3d(anf_33);
        float anf_35 = (anf_34 * 0.125);
        float anf_36 = (anf_32 + anf_35);
        vec3 anf_37 = (p_2 * 8.);
        float anf_38 = noise3d(anf_37);
        float anf_39 = (anf_38 * 0.0625);
        float anf_40 = (anf_36 + anf_39);
        vec3 anf_41 = (p_2 * 16.);
        float anf_42 = noise3d(anf_41);
        float anf_43 = (anf_42 * 0.03125);
        return (anf_40 + anf_43);
    }
    struct option {
        int tag;
        float Some_0;
    };
    vec2 rotate(vec2 p, float angle) {
        float s = sin(angle);
        float c = cos(angle);
        float anf_44 = p[0];
        float anf_45 = (anf_44 * c);
        float anf_46 = p[1];
        float anf_47 = (anf_46 * s);
        float anf_48 = (anf_45 - anf_47);
        float anf_49 = p[0];
        float anf_50 = (anf_49 * s);
        float anf_51 = p[1];
        float anf_52 = (anf_51 * c);
        float anf_53 = (anf_50 + anf_52);
        return vec2(anf_48, anf_53);
    }
    vec3 rotate_by_mouse_m_0(vec2 mouseUV, vec3 ray) {
        float anf_54 = mouseUV[1];
        float anf_55 = (-1. * anf_54);
        float rotX = (anf_55 * 1.5);
        float anf_56 = ray[1];
        float anf_57 = ray[2];
        vec2 anf_58 = vec2(anf_56, anf_57);
        vec2 ro_yz = rotate(anf_58, rotX);
        float anf_59 = mouseUV[0];
        float anf_60 = (-1. * anf_59);
        float rotY = (anf_60 * 1.5);
        float anf_61 = ray[0];
        float anf_62 = ro_yz[1];
        vec2 anf_63 = vec2(anf_61, anf_62);
        vec2 ro_xz = rotate(anf_63, rotY);
        float anf_64 = ro_xz[0];
        float anf_65 = ro_yz[0];
        float anf_66 = ro_xz[1];
        return vec3(anf_64, anf_65, anf_66);
    }
    float sdPlanet(vec3 p_3, float radius) {
        float len = length(p_3);
        vec3 dir = (p_3 / len);
        vec3 anf_67 = (dir * 3.);
        float anf_68 = fbm(anf_67);
        float terrain = (anf_68 * 0.4);
        float anf_69 = (len - radius);
        return (anf_69 - terrain);
    }
    float map(vec3 p_4) {
        return sdPlanet(p_4, 1.5);
    }
    vec3 getNormal(vec3 p_5) {
        float e_0 = 0.002;
        vec3 e_x = vec3(e_0, 0., 0.);
        vec3 e_y = vec3(0., e_0, 0.);
        vec3 e_z = vec3(0., 0., e_0);
        vec3 anf_70 = (p_5 + e_x);
        float anf_71 = map(anf_70);
        vec3 anf_72 = (p_5 - e_x);
        float anf_73 = map(anf_72);
        float dx = (anf_71 - anf_73);
        vec3 anf_74 = (p_5 + e_y);
        float anf_75 = map(anf_74);
        vec3 anf_76 = (p_5 - e_y);
        float anf_77 = map(anf_76);
        float dy = (anf_75 - anf_77);
        vec3 anf_78 = (p_5 + e_z);
        float anf_79 = map(anf_78);
        vec3 anf_80 = (p_5 - e_z);
        float anf_81 = map(anf_80);
        float dz = (anf_79 - anf_81);
        vec3 anf_82 = vec3(dx, dy, dz);
        return normalize(anf_82);
    }
    option march_0_0(vec3 rd, vec3 ro, float t, int steps) {
        int _iter = 0;
        while ((_iter < 1000)) {
            bool anf_83 = (steps > 120);
            if (anf_83) {
                return option(1, 0.);
            } else {
                vec3 anf_84 = (rd * t);
                vec3 anf_85 = (ro + anf_84);
                float d_1 = map(anf_85);
                bool anf_86 = (d_1 < 0.0005);
                if (anf_86) {
                    return option(0, t);
                } else {
                    bool anf_87 = (t > 50.);
                    if (anf_87) {
                        return option(1, 0.);
                    } else {
                        float anf_88 = (d_1 * 0.8);
                        float anf_89 = (t + anf_88);
                        int anf_90 = (steps + 1);
                        rd = rd;
                        ro = ro;
                        t = anf_89;
                        steps = anf_90;
                        int _iter_inc = (_iter + 1);
                        _iter = _iter_inc;
                        continue;
                    }
                }
            }
        }
        option _tmp;
        return _tmp;
    }
    option march(vec3 ro, vec3 rd) {
        return march_0_0(rd, ro, 0., 0);
    }
    const vec3 snowColor = vec3(0.85, 0.85, 0.9);
    uniform vec2 u_mouse;
    uniform vec2 u_resolution;
    vec3 main_pure(vec2 coord) {
        float anf_91 = u_resolution[0];
        float anf_92 = u_resolution[1];
        float res_min = min(anf_91, anf_92);
        vec2 anf_93 = (coord * 2.);
        vec2 anf_94 = (anf_93 - u_resolution);
        vec2 uv = (anf_94 / res_min);
        vec2 anf_95 = (u_mouse * 2.);
        vec2 anf_96 = (anf_95 - u_resolution);
        vec2 mouseUV = (anf_96 / res_min);
        vec3 anf_97 = vec3(0., 0., -4.);
        vec3 ro_0 = rotate_by_mouse_m_0(mouseUV, anf_97);
        float anf_98 = uv[0];
        float anf_99 = uv[1];
        vec3 anf_100 = vec3(anf_98, anf_99, 1.5);
        vec3 anf_101 = normalize(anf_100);
        vec3 rd_0 = rotate_by_mouse_m_0(mouseUV, anf_101);
        option t_0 = march(ro_0, rd_0);
        int _lv_tag = t_0.tag;
        switch (_lv_tag) {
            case 1: {
                return vec3(0., 0., 0.);
                break;
            }
            default: {
                float t_1 = t_0.Some_0;
                vec3 anf_102 = (rd_0 * t_1);
                vec3 hitPos = (ro_0 + anf_102);
                vec3 n = getNormal(hitPos);
                vec3 anf_103 = vec3(1., 0.8, -0.5);
                vec3 lightDir = normalize(anf_103);
                float anf_104 = dot(n, lightDir);
                float diff = max(anf_104, 0.);
                float ambient = 0.08;
                float anf_105 = length(hitPos);
                vec3 dir_0 = (hitPos / anf_105);
                vec3 anf_106 = (dir_0 * 3.);
                float rawHeight = fbm(anf_106);
                float seaLevel = 0.35;
                float anf_107 = (rawHeight - seaLevel);
                float anf_108 = (1. - seaLevel);
                float anf_109 = (anf_107 / anf_108);
                float h_norm = clamp(anf_109, 0., 1.);
                bool anf_110 = (h_norm < 0.3);
                vec3 baseColor;
                if (anf_110) {
                    float anf_111 = (h_norm / 0.3);
                    baseColor = mix(deepColor, landColor, anf_111);
                } else {
                    bool anf_112 = (h_norm < 0.6);
                    if (anf_112) {
                        float anf_113 = (h_norm - 0.3);
                        float anf_114 = (anf_113 / 0.3);
                        baseColor = mix(landColor, mountColor, anf_114);
                    } else {
                        float anf_115 = (h_norm - 0.6);
                        float anf_116 = (anf_115 / 0.4);
                        baseColor = mix(mountColor, snowColor, anf_116);
                    }
                }
                vec3 anf_117 = (rd_0 * -1.);
                float anf_118 = dot(n, anf_117);
                float anf_119 = max(anf_118, 0.);
                float fresnel = (1. - anf_119);
                float anf_120 = (fresnel * fresnel);
                float anf_121 = (anf_120 * fresnel);
                float rim = (anf_121 * 0.4);
                vec3 atmoColor = vec3(0.3, 0.5, 1.);
                float anf_122 = (diff * 0.9);
                float anf_123 = (anf_122 + ambient);
                vec3 anf_124 = (baseColor * anf_123);
                vec3 anf_125 = (atmoColor * rim);
                return (anf_124 + anf_125);
                break;
            }
        }
    }
    uniform float u_time;
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }


    ====== COMPILING EXAMPLE rainbow.glml ======

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    uniform vec2 u_resolution;
    vec2 get_uv_m(vec2 coord) {
        vec2 anf = (2. * coord);
        vec2 top = (anf - u_resolution);
        float anf_0 = u_resolution[0];
        float anf_1 = u_resolution[1];
        float bot = min(anf_0, anf_1);
        return (top / bot);
    }
    uniform float u_time;
    vec3 main_pure(vec2 coord_0) {
        vec2 uv = get_uv_m(coord_0);
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
    vec3 palette(float t) {
        vec3 cfg = vec3(0.3, 0.416, 0.557);
        vec3 anf = (cfg + t);
        vec3 anf_0 = (anf * 6.28318);
        vec3 anf_1 = cos(anf_0);
        vec3 anf_2 = (anf_1 * 0.5);
        return (anf_2 + 0.5);
    }
    vec2 rotate(vec2 p, float angle) {
        float s = sin(angle);
        float c = cos(angle);
        float anf_3 = p[0];
        float anf_4 = (anf_3 * c);
        float anf_5 = p[1];
        float anf_6 = (anf_5 * s);
        float anf_7 = (anf_4 - anf_6);
        float anf_8 = p[0];
        float anf_9 = (anf_8 * s);
        float anf_10 = p[1];
        float anf_11 = (anf_10 * c);
        float anf_12 = (anf_9 + anf_11);
        return vec2(anf_7, anf_12);
    }
    float sMin(float a, float b) {
        float k = 0.1;
        float anf_13 = (b - a);
        float anf_14 = (0.5 * anf_13);
        float anf_15 = (anf_14 / k);
        float anf_16 = (0.5 + anf_15);
        float h = clamp(anf_16, 0., 1.);
        float anf_17 = mix(b, a, h);
        float anf_18 = (k * h);
        float anf_19 = (1. - h);
        float anf_20 = (anf_18 * anf_19);
        return (anf_17 - anf_20);
    }
    float sdTorus(vec3 p_0, vec2 t_0) {
        float anf_21 = p_0[0];
        float anf_22 = p_0[2];
        vec2 anf_23 = vec2(anf_21, anf_22);
        float anf_24 = length(anf_23);
        float anf_25 = t_0[0];
        float anf_26 = (anf_24 - anf_25);
        float anf_27 = p_0[1];
        vec2 q = vec2(anf_26, anf_27);
        float anf_28 = length(q);
        float anf_29 = t_0[1];
        return (anf_28 - anf_29);
    }
    uniform vec2 u_mouse;
    uniform vec2 u_resolution;
    uniform float u_time;
    float map(vec3 p_1) {
        float angle_0 = (u_time * 2.);
        float anf_30 = p_1[0];
        float anf_31 = p_1[1];
        vec2 anf_32 = vec2(anf_30, anf_31);
        vec2 p_xy = rotate(anf_32, angle_0);
        float anf_33 = p_xy[0];
        float anf_34 = p_xy[1];
        float anf_35 = p_1[2];
        vec3 p_prime = vec3(anf_33, anf_34, anf_35);
        float anf_36 = p_prime[1];
        float anf_37 = p_prime[2];
        vec2 anf_38 = vec2(anf_36, anf_37);
        vec2 p_yz = rotate(anf_38, angle_0);
        float anf_39 = p_prime[0];
        float anf_40 = p_yz[0];
        float anf_41 = p_yz[1];
        vec3 p_prime_0 = vec3(anf_39, anf_40, anf_41);
        vec2 anf_42 = vec2(1., 0.3);
        float anf_43 = sdTorus(p_prime_0, anf_42);
        vec2 anf_44 = vec2(2., 0.5);
        float anf_45 = sdTorus(p_1, anf_44);
        return sMin(anf_43, anf_45);
    }
    option march_0_0(vec3 rd, vec3 ro, float t_1, int steps) {
        int _iter = 0;
        while ((_iter < 1000)) {
            bool anf_46 = (steps > 80);
            if (anf_46) {
                return option(1, 0.);
            } else {
                vec3 anf_47 = (rd * t_1);
                vec3 anf_48 = (ro + anf_47);
                float d = map(anf_48);
                bool anf_49 = (d < 0.001);
                if (anf_49) {
                    return option(0, t_1);
                } else {
                    bool anf_50 = (t_1 > 100.);
                    if (anf_50) {
                        return option(1, 0.);
                    } else {
                        float anf_51 = (t_1 + d);
                        int anf_52 = (steps + 1);
                        rd = rd;
                        ro = ro;
                        t_1 = anf_51;
                        steps = anf_52;
                        int _iter_inc = (_iter + 1);
                        _iter = _iter_inc;
                        continue;
                    }
                }
            }
        }
        option _tmp;
        return _tmp;
    }
    option march(vec3 ro, vec3 rd) {
        return march_0_0(rd, ro, 0., 0);
    }
    vec3 main_pure(vec2 coord) {
        float anf_53 = u_resolution[0];
        float anf_54 = u_resolution[1];
        float res_min = min(anf_53, anf_54);
        vec2 anf_55 = (coord * 2.);
        vec2 anf_56 = (anf_55 - u_resolution);
        vec2 uv = (anf_56 / res_min);
        vec2 anf_57 = (u_mouse * 2.);
        vec2 anf_58 = (anf_57 - u_resolution);
        vec2 mouseUV = (anf_58 / res_min);
        vec3 ro_init = vec3(0., 0., -6.);
        float anf_59 = uv[0];
        float anf_60 = uv[1];
        vec3 anf_61 = vec3(anf_59, anf_60, 1.);
        vec3 rd_init = normalize(anf_61);
        float anf_62 = mouseUV[1];
        float rotX = (-1. * anf_62);
        float anf_63 = mouseUV[0];
        float rotY = (-1. * anf_63);
        float anf_64 = ro_init[1];
        float anf_65 = ro_init[2];
        vec2 anf_66 = vec2(anf_64, anf_65);
        vec2 ro_yz = rotate(anf_66, rotX);
        float anf_67 = rd_init[1];
        float anf_68 = rd_init[2];
        vec2 anf_69 = vec2(anf_67, anf_68);
        vec2 rd_yz = rotate(anf_69, rotX);
        float anf_70 = ro_init[0];
        float anf_71 = ro_yz[0];
        float anf_72 = ro_yz[1];
        vec3 ro_0 = vec3(anf_70, anf_71, anf_72);
        float anf_73 = rd_init[0];
        float anf_74 = rd_yz[0];
        float anf_75 = rd_yz[1];
        vec3 rd_0 = vec3(anf_73, anf_74, anf_75);
        float anf_76 = ro_0[0];
        float anf_77 = ro_0[2];
        vec2 anf_78 = vec2(anf_76, anf_77);
        vec2 ro_xz = rotate(anf_78, rotY);
        float anf_79 = rd_0[0];
        float anf_80 = rd_0[2];
        vec2 anf_81 = vec2(anf_79, anf_80);
        vec2 rd_xz = rotate(anf_81, rotY);
        float anf_82 = ro_xz[0];
        float anf_83 = ro_0[1];
        float anf_84 = ro_xz[1];
        vec3 ro_1 = vec3(anf_82, anf_83, anf_84);
        float anf_85 = rd_xz[0];
        float anf_86 = rd_0[1];
        float anf_87 = rd_xz[1];
        vec3 rd_1 = vec3(anf_85, anf_86, anf_87);
        option anf_88 = march(ro_1, rd_1);
        int _lv_tag = anf_88.tag;
        vec3 col;
        switch (_lv_tag) {
            case 1: {
                col = vec3(0.2, 0.2, 0.2);
                break;
            }
            default: {
                float t_2 = anf_88.Some_0;
                float anf_89 = (t_2 * 0.3);
                col = palette(anf_89);
                break;
            }
        }
        vec2 anf_90 = (uv - mouseUV);
        float anf_91 = length(anf_90);
        float glow = (0.02 / anf_91);
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
        while ((_iter < 1000)) {
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
                        a = anf_2;
                        b = b;
                        int _iter_inc_0 = (_iter + 1);
                        _iter = _iter_inc_0;
                        continue;
                    } else {
                        float anf_3 = (b - a);
                        a = a;
                        b = anf_3;
                        int _iter_inc = (_iter + 1);
                        _iter = _iter_inc;
                        continue;
                    }
                }
            }
        }
        return 0.;
    }
    mat2 rotate(float angle) {
        float s = sin(angle);
        float c = cos(angle);
        float anf_4 = (-1. * s);
        vec2 anf_5 = vec2(c, anf_4);
        vec2 anf_6 = vec2(s, c);
        return mat2(anf_5, anf_6);
    }
    uniform vec2 u_resolution;
    vec2 get_uv(vec2 coord) {
        vec2 anf_7 = (2. * coord);
        vec2 top = (anf_7 - u_resolution);
        float anf_8 = u_resolution[0];
        float anf_9 = u_resolution[1];
        float bot = min(anf_8, anf_9);
        return (top / bot);
    }
    uniform float u_time;
    vec3 main_pure(vec2 coord_0) {
        mat2 anf_10 = rotate(u_time);
        vec2 anf_11 = get_uv(coord_0);
        vec2 uv = (anf_10 * anf_11);
        float anf_12 = (u_time * 2.);
        float anf_13 = sin(anf_12);
        vec2 anf_14 = (uv * anf_13);
        vec2 anf_15 = (anf_14 * 2.);
        vec2 anf_16 = abs(anf_15);
        float x = anf_16[0];
        float y = anf_16[1];
        float res = gcd_m(x, y);
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
        vec2 anf = (pf * pf);
        vec2 anf_0 = (2. * pf);
        vec2 anf_1 = (3. - anf_0);
        vec2 inter = (anf * anf_1);
        vec4 v4 = vec4(0., 1., 27., 28.);
        float anf_2 = i[0];
        vec4 anf_3 = (v4 + anf_2);
        float anf_4 = i[1];
        float anf_5 = (anf_4 * 27.);
        vec4 seed = (anf_3 + anf_5);
        vec4 anf_6 = mod(seed, 6.2831853);
        vec4 anf_7 = sin(anf_6);
        vec4 anf_8 = (anf_7 * 200000.);
        vec4 hash = fract(anf_8);
        float anf_9 = hash[0];
        float anf_10 = hash[1];
        vec2 col0 = vec2(anf_9, anf_10);
        float anf_11 = hash[2];
        float anf_12 = hash[3];
        vec2 col1 = vec2(anf_11, anf_12);
        float anf_13 = inter[1];
        float anf_14 = (1. - anf_13);
        vec2 anf_15 = (col0 * anf_14);
        float anf_16 = inter[1];
        vec2 anf_17 = (col1 * anf_16);
        vec2 res_v = (anf_15 + anf_17);
        float anf_18 = inter[0];
        float anf_19 = (1. - anf_18);
        float anf_20 = inter[0];
        vec2 anf_21 = vec2(anf_19, anf_20);
        return dot(res_v, anf_21);
    }
    float fractalNoise(vec2 p_0) {
        float anf_22 = smoothNoise(p_0);
        float anf_23 = (anf_22 * 0.5333);
        vec2 anf_24 = (p_0 * 2.);
        float anf_25 = smoothNoise(anf_24);
        float anf_26 = (anf_25 * 0.2667);
        float anf_27 = (anf_23 + anf_26);
        vec2 anf_28 = (p_0 * 4.);
        float anf_29 = smoothNoise(anf_28);
        float anf_30 = (anf_29 * 0.1333);
        float anf_31 = (anf_27 + anf_30);
        vec2 anf_32 = (p_0 * 8.);
        float anf_33 = smoothNoise(anf_32);
        float anf_34 = (anf_33 * 0.0667);
        return (anf_31 + anf_34);
    }
    uniform vec2 u_resolution;
    uniform float u_time;
    float warpedNoise(vec2 p_1) {
        float anf_35 = (-1. * u_time);
        vec2 anf_36 = vec2(u_time, anf_35);
        vec2 m = (anf_36 * 0.5);
        vec2 anf_37 = (p_1 + m);
        float x = fractalNoise(anf_37);
        float anf_38 = m[1];
        float anf_39 = m[0];
        vec2 anf_40 = vec2(anf_38, anf_39);
        vec2 anf_41 = (p_1 + anf_40);
        vec2 anf_42 = (anf_41 + x);
        float y = fractalNoise(anf_42);
        vec2 anf_43 = (p_1 - m);
        vec2 anf_44 = (anf_43 - x);
        vec2 anf_45 = (anf_44 + y);
        float z = fractalNoise(anf_45);
        vec2 anf_46 = vec2(x, y);
        vec2 anf_47 = vec2(y, z);
        vec2 anf_48 = (anf_46 + anf_47);
        vec2 anf_49 = vec2(z, x);
        vec2 warp = (anf_48 + anf_49);
        vec3 anf_50 = vec3(x, y, z);
        float anf_51 = length(anf_50);
        float mag = (anf_51 * 0.25);
        vec2 anf_52 = (p_1 + warp);
        vec2 anf_53 = (anf_52 + mag);
        return fractalNoise(anf_53);
    }
    vec3 main_pure(vec2 coord) {
        vec2 anf_54 = (u_resolution * 0.5);
        vec2 anf_55 = (coord - anf_54);
        float anf_56 = u_resolution[1];
        vec2 uv = (anf_55 / anf_56);
        vec2 anf_57 = (uv * 6.);
        float n = warpedNoise(anf_57);
        vec2 anf_58 = (uv * 6.);
        vec2 anf_59 = (anf_58 - 0.02);
        float n2 = warpedNoise(anf_59);
        float anf_60 = (n2 - n);
        float anf_61 = max(anf_60, 0.);
        float anf_62 = (anf_61 / 0.02);
        float bump = (anf_62 * 0.7071);
        float anf_63 = (n - n2);
        float anf_64 = max(anf_63, 0.);
        float anf_65 = (anf_64 / 0.02);
        float bump2 = (anf_65 * 0.7071);
        float anf_66 = (bump * bump);
        float anf_67 = pow(bump, 4.);
        float anf_68 = (anf_67 * 0.5);
        float b1 = (anf_66 + anf_68);
        float anf_69 = (bump2 * bump2);
        float anf_70 = pow(bump2, 4.);
        float anf_71 = (anf_70 * 0.5);
        float b2 = (anf_69 + anf_71);
        vec3 anf_72 = vec3(1., 0.7, 0.6);
        float anf_73 = (b1 + b2);
        float anf_74 = (anf_73 * 0.4);
        vec3 anf_75 = vec3(b1, anf_74, b2);
        vec3 anf_76 = (anf_72 * anf_75);
        vec3 anf_77 = (anf_76 * 0.3);
        vec3 base_col = (anf_77 + 0.5);
        float anf_78 = (n * n);
        vec3 col = (anf_78 * base_col);
        vec2 anf_79 = (uv - 0.65);
        float spot1_dist = length(anf_79);
        vec2 anf_80 = (uv + 0.5);
        float spot2_dist = length(anf_80);
        vec3 anf_81 = vec3(0.8, 0.4, 1.);
        vec3 anf_82 = (anf_81 * 0.35);
        vec3 anf_83 = vec3(1., 0.5, 0.2);
        float anf_84 = (1. - spot1_dist);
        float anf_85 = smoothstep(0., 1., anf_84);
        vec3 anf_86 = (anf_83 * anf_85);
        vec3 anf_87 = vec3(0.2, 0.4, 1.);
        float anf_88 = (1. - spot2_dist);
        float anf_89 = smoothstep(0., 1., anf_88);
        vec3 anf_90 = (anf_87 * anf_89);
        vec3 anf_91 = (anf_86 + anf_90);
        vec3 anf_92 = (anf_91 * 5.);
        vec3 spot_logic = (anf_82 + anf_92);
        vec3 final_col = (col * spot_logic);
        vec3 anf_93 = max(final_col, 0.);
        return sqrt(anf_93);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;
