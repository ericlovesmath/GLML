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
    const vec3 blue_19 = vec3(0.65, 0.85, 1.);
    const vec3 orange_18 = vec3(0.9, 0.6, 0.3);
    struct shape {
        int tag;
        float Circle_0;
        float Rect_0;
        float Rect_1;
    };
    float sdf_shape_0(shape s_1, vec2 p_2) {
        int _lv_tag_225 = s_1.tag;
        switch (_lv_tag_225) {
            case 0: {
                float r_3 = s_1.Circle_0;
                float anf_187 = length(p_2);
                return (anf_187 - r_3);
                break;
            }
            case 1: {
                float w_4 = s_1.Rect_0;
                float h_5 = s_1.Rect_1;
                vec2 anf_188 = abs(p_2);
                vec2 anf_189 = vec2(w_4, h_5);
                vec2 d_6 = (anf_188 - anf_189);
                vec2 anf_190 = vec2(0., 0.);
                vec2 anf_191 = max(d_6, anf_190);
                float anf_192 = length(anf_191);
                float anf_193 = d_6[0];
                float anf_194 = d_6[1];
                float anf_195 = max(anf_193, anf_194);
                float anf_196 = min(anf_195, 0.);
                return (anf_192 + anf_196);
                break;
            }
            default: {
                return 1.;
                break;
            }
        }
    }
    struct DFn_170 {
        int tag;
        shape lctor_173_0;
        shape lctor_176_0;
    };
    struct DFn_181 {
        int tag;
        DFn_170 lctor_182_0;
        DFn_170 lctor_182_1;
    };
    DFn_181 scene_11() {
        shape anf_197 = shape(0, 0.3, 0., 0.);
        shape _tmp_227;
        DFn_170 circle_12 = DFn_170(0, anf_197, _tmp_227);
        shape anf_198 = shape(1, 0., 0.7, 0.1);
        shape _tmp_228;
        DFn_170 rect_13 = DFn_170(1, _tmp_228, anf_198);
        return DFn_181(0, circle_12, rect_13);
    }
    float dapply_169(DFn_170 dfn_183, vec2 da_184) {
        int _lv_tag_226 = dfn_183.tag;
        switch (_lv_tag_226) {
            case 0: {
                shape ca_172 = dfn_183.lctor_173_0;
                return sdf_shape_0(ca_172, da_184);
                break;
            }
            default: {
                shape ca_175 = dfn_183.lctor_176_0;
                return sdf_shape_0(ca_175, da_184);
                break;
            }
        }
    }
    uniform vec2 u_mouse;
    uniform vec2 u_resolution;
    vec2 get_uv_14_vec2_to_vec2_165(vec2 coord_15) {
        vec2 anf_199 = (2. * coord_15);
        vec2 top_16 = (anf_199 - u_resolution);
        float anf_200 = u_resolution[0];
        float anf_201 = u_resolution[1];
        float bot_17 = min(anf_200, anf_201);
        return (top_16 / bot_17);
    }
    float union_7(DFn_170 f_8, DFn_170 f_prime_9, vec2 p_10) {
        float anf_202 = dapply_169(f_8, p_10);
        float anf_203 = dapply_169(f_prime_9, p_10);
        return min(anf_202, anf_203);
    }
    float dapply_180(DFn_181 dfn_185, vec2 da_186) {
        DFn_170 ca_178 = dfn_185.lctor_182_0;
        DFn_170 ca_179 = dfn_185.lctor_182_1;
        return union_7(ca_178, ca_179, da_186);
    }
    vec3 main_pure(vec2 coord_20) {
        vec2 p_21 = get_uv_14_vec2_to_vec2_165(coord_20);
        vec2 m_22 = get_uv_14_vec2_to_vec2_165(u_mouse);
        DFn_181 _lc_229 = scene_11();
        float d_23 = dapply_180(_lc_229, p_21);
        bool anf_204 = (d_23 > 0.);
        vec3 col_24;
        if (anf_204) {
            col_24 = orange_18;
        } else {
            col_24 = blue_19;
        }
        float anf_205 = abs(d_23);
        float anf_206 = (-6. * anf_205);
        float anf_207 = exp(anf_206);
        float darken_26 = (1. - anf_207);
        float anf_208 = (150. * d_23);
        float anf_209 = cos(anf_208);
        float anf_210 = (0.2 * anf_209);
        float rings_27 = (0.8 + anf_210);
        vec3 anf_211 = (col_24 * darken_26);
        vec3 col_25 = (anf_211 * rings_27);
        vec3 anf_212 = vec3(1., 1., 1.);
        float anf_213 = abs(d_23);
        float anf_214 = smoothstep(0., 0.01, anf_213);
        float anf_215 = (1. - anf_214);
        vec3 col_28 = mix(col_25, anf_212, anf_215);
        DFn_181 _lc_230 = scene_11();
        float anf_216 = dapply_180(_lc_230, m_22);
        float d_30 = abs(anf_216);
        vec2 anf_217 = (p_21 - m_22);
        float dm_31 = length(anf_217);
        float anf_218 = (dm_31 - d_30);
        float anf_219 = abs(anf_218);
        float anf_220 = (anf_219 - 0.0025);
        float anf_221 = (dm_31 - 0.015);
        float d_32 = min(anf_220, anf_221);
        vec3 anf_222 = vec3(1., 1., 0.);
        float anf_223 = smoothstep(0., 0.005, d_32);
        float anf_224 = (1. - anf_223);
        vec3 col_29 = mix(col_28, anf_222, anf_224);
        return col_29;
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
    vec2 at_17(vec2 offset_18, vec2 p_19) {
        return (p_19 - offset_18);
    }
    const vec3 bg_col_0 = vec3(0.13, 0.48, 0.3);
    const vec3 bg_dark_1 = vec3(0.09, 0.36, 0.22);
    const vec3 black_10 = vec3(0.06, 0.04, 0.03);
    float box_26(vec2 b_27, vec2 p_28) {
        vec2 anf_803 = abs(p_28);
        vec2 d_29 = (anf_803 - b_27);
        vec2 anf_804 = vec2(0., 0.);
        vec2 anf_805 = max(d_29, anf_804);
        float anf_806 = length(anf_805);
        float anf_807 = d_29[0];
        float anf_808 = d_29[1];
        float anf_809 = max(anf_807, anf_808);
        float anf_810 = min(anf_809, 0.);
        return (anf_806 + anf_810);
    }
    const vec3 brown_2 = vec3(0.55, 0.34, 0.16);
    const vec3 brown_dk_4 = vec3(0.38, 0.22, 0.08);
    const vec3 brown_lt_3 = vec3(0.68, 0.46, 0.24);
    float circle_23(float r_24, vec2 p_25) {
        float anf_811 = length(p_25);
        return (anf_811 - r_24);
    }
    const vec3 cream_6 = vec3(0.95, 0.89, 0.74);
    const vec3 cream_dk_7 = vec3(0.78, 0.68, 0.5);
    const vec3 drk_brown_5 = vec3(0.22, 0.11, 0.03);
    float ellipse_20(vec2 ab_21, vec2 p_22) {
        vec2 anf_812 = (p_22 / ab_21);
        float anf_813 = length(anf_812);
        return (anf_813 - 1.);
    }
    vec3 paint_45(float d_46, vec3 shape_col_47, vec3 bg_48) {
        float anf_814 = smoothstep(-0.005, 0.005, d_46);
        return mix(shape_col_47, bg_48, anf_814);
    }
    vec3 paint_shaded_49(float d_50, vec3 shape_col_51, vec3 shadow_col_52, float shadow_depth_53, vec3 bg_54) {
        float anf_815 = smoothstep(-0.005, 0.005, d_50);
        vec3 base_55 = mix(shape_col_51, bg_54, anf_815);
        float anf_816 = (-1. * shadow_depth_53);
        float shade_56 = smoothstep(anf_816, 0., d_50);
        float anf_817 = (shade_56 * 0.55);
        vec3 anf_818 = mix(shape_col_51, shadow_col_52, anf_817);
        float anf_819 = smoothstep(0.005, -0.005, d_50);
        return mix(base_55, anf_818, anf_819);
    }
    const vec3 pink_8 = vec3(0.92, 0.62, 0.6);
    vec2 rot_30(float a_31, vec2 p_32) {
        float c_33 = cos(a_31);
        float s_34 = sin(a_31);
        float anf_820 = p_32[0];
        float anf_821 = (c_33 * anf_820);
        float anf_822 = p_32[1];
        float anf_823 = (s_34 * anf_822);
        float anf_824 = (anf_821 - anf_823);
        float anf_825 = p_32[0];
        float anf_826 = (s_34 * anf_825);
        float anf_827 = p_32[1];
        float anf_828 = (c_33 * anf_827);
        float anf_829 = (anf_826 + anf_828);
        return vec2(anf_824, anf_829);
    }
    float smin_35(float a_36, float b_37, float k_38) {
        float anf_830 = (a_36 - b_37);
        float anf_831 = abs(anf_830);
        float anf_832 = (k_38 - anf_831);
        float anf_833 = max(anf_832, 0.);
        float h_39 = (anf_833 / k_38);
        float anf_834 = min(a_36, b_37);
        float anf_835 = (h_39 * h_39);
        float anf_836 = (anf_835 * k_38);
        float anf_837 = (anf_836 * 0.25);
        return (anf_834 - anf_837);
    }
    const vec3 tooth_yel_12 = vec3(0.92, 0.85, 0.6);
    uniform vec2 u_resolution;
    vec2 get_uv_13_vec2_to_vec2_799(vec2 coord_14) {
        vec2 anf_838 = (2. * coord_14);
        vec2 top_15 = (anf_838 - u_resolution);
        float anf_839 = u_resolution[0];
        float anf_840 = u_resolution[1];
        float bot_16 = min(anf_839, anf_840);
        return (top_15 / bot_16);
    }
    const vec3 wht_11 = vec3(1., 0.97, 0.93);
    vec3 main_pure(vec2 coord_57) {
        vec2 anf_841 = get_uv_13_vec2_to_vec2_799(coord_57);
        vec2 p_58 = (anf_841 / 1.5);
        vec2 anf_842 = vec2(0.28, -0.28);
        vec2 anf_843 = at_17(anf_842, p_58);
        vec2 tp_59 = rot_30(-0.35, anf_843);
        vec2 anf_844 = vec2(0.22, 0.085);
        float tail_60 = ellipse_20(anf_844, tp_59);
        float anf_845 = tp_59[0];
        float anf_846 = tp_59[1];
        float anf_847 = (anf_845 + anf_846);
        float anf_848 = (anf_847 * 38.);
        float anf_849 = sin(anf_848);
        float anf_850 = abs(anf_849);
        float tsx_61 = (anf_850 - 0.55);
        float anf_851 = tp_59[0];
        float anf_852 = tp_59[1];
        float anf_853 = (anf_851 - anf_852);
        float anf_854 = (anf_853 * 38.);
        float anf_855 = sin(anf_854);
        float anf_856 = abs(anf_855);
        float tsy_62 = (anf_856 - 0.55);
        float anf_857 = (tail_60 + 0.01);
        float anf_858 = min(tsx_61, tsy_62);
        float anf_859 = (anf_858 * 0.015);
        float tail_scales_63 = max(anf_857, anf_859);
        float anf_860 = (tail_60 + 0.018);
        float anf_861 = (-1. * anf_860);
        float tail_rim_64 = max(tail_60, anf_861);
        vec2 anf_862 = vec2(0.25, 0.26);
        vec2 anf_863 = vec2(0., -0.16);
        vec2 anf_864 = at_17(anf_863, p_58);
        float body_65 = ellipse_20(anf_862, anf_864);
        vec2 anf_865 = vec2(0.22, 0.2);
        vec2 anf_866 = vec2(0., 0.2);
        vec2 anf_867 = at_17(anf_866, p_58);
        float head_66 = ellipse_20(anf_865, anf_867);
        float torso_67 = smin_35(body_65, head_66, 0.1);
        vec2 anf_868 = vec2(-0.14, 0.11);
        vec2 anf_869 = at_17(anf_868, p_58);
        float cheek_l_68 = circle_23(0.1, anf_869);
        vec2 anf_870 = vec2(0.14, 0.11);
        vec2 anf_871 = at_17(anf_870, p_58);
        float cheek_r_69 = circle_23(0.1, anf_871);
        float cheeks_70 = min(cheek_l_68, cheek_r_69);
        float torso_with_cheeks_71 = smin_35(torso_67, cheeks_70, 0.06);
        vec2 anf_872 = vec2(0.13, 0.095);
        vec2 anf_873 = vec2(0., 0.09);
        vec2 anf_874 = at_17(anf_873, p_58);
        float muzzle_72 = ellipse_20(anf_872, anf_874);
        vec2 anf_875 = vec2(0.15, 0.17);
        vec2 anf_876 = vec2(0., -0.2);
        vec2 anf_877 = at_17(anf_876, p_58);
        float belly_73 = ellipse_20(anf_875, anf_877);
        vec2 anf_878 = vec2(-0.175, 0.355);
        vec2 anf_879 = at_17(anf_878, p_58);
        float ear_l_74 = circle_23(0.075, anf_879);
        vec2 anf_880 = vec2(0.175, 0.355);
        vec2 anf_881 = at_17(anf_880, p_58);
        float ear_r_75 = circle_23(0.075, anf_881);
        vec2 anf_882 = vec2(0.035, 0.042);
        vec2 anf_883 = vec2(-0.175, 0.345);
        vec2 anf_884 = at_17(anf_883, p_58);
        float ear_in_l_76 = ellipse_20(anf_882, anf_884);
        vec2 anf_885 = vec2(0.035, 0.042);
        vec2 anf_886 = vec2(0.175, 0.345);
        vec2 anf_887 = at_17(anf_886, p_58);
        float ear_in_r_77 = ellipse_20(anf_885, anf_887);
        vec2 anf_888 = vec2(0.065, 0.09);
        vec2 anf_889 = vec2(-0.23, -0.09);
        vec2 anf_890 = at_17(anf_889, p_58);
        float arm_l_78 = ellipse_20(anf_888, anf_890);
        vec2 anf_891 = vec2(0.065, 0.09);
        vec2 anf_892 = vec2(0.23, -0.09);
        vec2 anf_893 = at_17(anf_892, p_58);
        float arm_r_79 = ellipse_20(anf_891, anf_893);
        vec2 anf_894 = vec2(-0.28, -0.19);
        vec2 anf_895 = at_17(anf_894, p_58);
        float paw_l_80 = circle_23(0.055, anf_895);
        vec2 anf_896 = vec2(0.28, -0.19);
        vec2 anf_897 = at_17(anf_896, p_58);
        float paw_r_81 = circle_23(0.055, anf_897);
        vec2 anf_898 = vec2(0.095, 0.048);
        vec2 anf_899 = vec2(-0.13, -0.42);
        vec2 anf_900 = at_17(anf_899, p_58);
        float foot_l_82 = ellipse_20(anf_898, anf_900);
        vec2 anf_901 = vec2(0.095, 0.048);
        vec2 anf_902 = vec2(0.13, -0.42);
        vec2 anf_903 = at_17(anf_902, p_58);
        float foot_r_83 = ellipse_20(anf_901, anf_903);
        vec2 anf_904 = vec2(0.022, 0.05);
        vec2 anf_905 = vec2(-0.028, 0.035);
        vec2 anf_906 = at_17(anf_905, p_58);
        float tooth_l_84 = box_26(anf_904, anf_906);
        vec2 anf_907 = vec2(0.022, 0.05);
        vec2 anf_908 = vec2(0.028, 0.035);
        vec2 anf_909 = at_17(anf_908, p_58);
        float tooth_r_85 = box_26(anf_907, anf_909);
        float teeth_86 = min(tooth_l_84, tooth_r_85);
        vec2 anf_910 = vec2(0.005, 0.05);
        vec2 anf_911 = vec2(0., 0.035);
        vec2 anf_912 = at_17(anf_911, p_58);
        float groove_87 = box_26(anf_910, anf_912);
        vec2 anf_913 = vec2(0.038, 0.028);
        vec2 anf_914 = vec2(0., 0.135);
        vec2 anf_915 = at_17(anf_914, p_58);
        float nose_88 = ellipse_20(anf_913, anf_915);
        vec2 anf_916 = vec2(0.012, 0.008);
        vec2 anf_917 = vec2(-0.012, 0.142);
        vec2 anf_918 = at_17(anf_917, p_58);
        float nose_hi_89 = ellipse_20(anf_916, anf_918);
        vec2 anf_919 = vec2(-0.095, 0.255);
        vec2 anf_920 = at_17(anf_919, p_58);
        float eye_l_90 = circle_23(0.04, anf_920);
        vec2 anf_921 = vec2(0.095, 0.255);
        vec2 anf_922 = at_17(anf_921, p_58);
        float eye_r_91 = circle_23(0.04, anf_922);
        vec2 anf_923 = vec2(-0.082, 0.27);
        vec2 anf_924 = at_17(anf_923, p_58);
        float hi_l_92 = circle_23(0.014, anf_924);
        vec2 anf_925 = vec2(0.108, 0.27);
        vec2 anf_926 = at_17(anf_925, p_58);
        float hi_r_93 = circle_23(0.014, anf_926);
        vec2 anf_927 = vec2(-0.105, 0.245);
        vec2 anf_928 = at_17(anf_927, p_58);
        float hi_l2_94 = circle_23(0.006, anf_928);
        vec2 anf_929 = vec2(0.085, 0.245);
        vec2 anf_930 = at_17(anf_929, p_58);
        float hi_r2_95 = circle_23(0.006, anf_930);
        vec2 anf_931 = vec2(0.035, 0.01);
        vec2 anf_932 = vec2(-0.095, 0.315);
        vec2 anf_933 = at_17(anf_932, p_58);
        vec2 anf_934 = rot_30(0.15, anf_933);
        float brow_l_96 = ellipse_20(anf_931, anf_934);
        vec2 anf_935 = vec2(0.035, 0.01);
        vec2 anf_936 = vec2(0.095, 0.315);
        vec2 anf_937 = at_17(anf_936, p_58);
        vec2 anf_938 = rot_30(-0.15, anf_937);
        float brow_r_97 = ellipse_20(anf_935, anf_938);
        float anf_939 = length(p_58);
        float vig_98 = smoothstep(0.3, 1.1, anf_939);
        vec3 col_99 = mix(bg_col_0, bg_dark_1, vig_98);
        vec2 anf_940 = vec2(0.38, 0.055);
        vec2 anf_941 = vec2(0.02, -0.45);
        vec2 anf_942 = at_17(anf_941, p_58);
        float shadow_d_100 = ellipse_20(anf_940, anf_942);
        float shadow_falloff_101 = smoothstep(0.08, -0.02, shadow_d_100);
        vec3 anf_943 = vec3(0.06, 0.28, 0.16);
        float anf_944 = (shadow_falloff_101 * 0.55);
        vec3 col_102 = mix(col_99, anf_943, anf_944);
        vec3 anf_945 = paint_45(tail_60, brown_dk_4, col_102);
        vec3 anf_946 = paint_45(tail_rim_64, drk_brown_5, anf_945);
        vec3 anf_947 = paint_45(tail_scales_63, drk_brown_5, anf_946);
        vec3 anf_948 = paint_45(foot_l_82, drk_brown_5, anf_947);
        vec3 anf_949 = paint_45(foot_r_83, drk_brown_5, anf_948);
        vec3 anf_950 = paint_45(arm_l_78, brown_dk_4, anf_949);
        vec3 anf_951 = paint_45(arm_r_79, brown_dk_4, anf_950);
        vec3 anf_952 = paint_shaded_49(torso_with_cheeks_71, brown_2, brown_dk_4, 0.08, anf_951);
        vec3 anf_953 = paint_shaded_49(belly_73, cream_6, cream_dk_7, 0.05, anf_952);
        vec3 anf_954 = paint_45(paw_l_80, cream_6, anf_953);
        vec3 anf_955 = paint_45(paw_r_81, cream_6, anf_954);
        vec3 anf_956 = paint_45(ear_l_74, brown_2, anf_955);
        vec3 anf_957 = paint_45(ear_r_75, brown_2, anf_956);
        vec3 anf_958 = paint_45(ear_in_l_76, pink_8, anf_957);
        vec3 anf_959 = paint_45(ear_in_r_77, pink_8, anf_958);
        vec3 anf_960 = paint_45(muzzle_72, cream_6, anf_959);
        vec3 anf_961 = paint_45(brow_l_96, drk_brown_5, anf_960);
        vec3 anf_962 = paint_45(brow_r_97, drk_brown_5, anf_961);
        vec3 anf_963 = paint_45(teeth_86, tooth_yel_12, anf_962);
        vec3 anf_964 = paint_45(groove_87, brown_dk_4, anf_963);
        vec3 anf_965 = paint_45(nose_88, drk_brown_5, anf_964);
        vec3 anf_966 = paint_45(nose_hi_89, brown_lt_3, anf_965);
        vec3 anf_967 = paint_45(eye_l_90, black_10, anf_966);
        vec3 anf_968 = paint_45(eye_r_91, black_10, anf_967);
        vec3 anf_969 = paint_45(hi_l_92, wht_11, anf_968);
        vec3 anf_970 = paint_45(hi_r_93, wht_11, anf_969);
        vec3 anf_971 = paint_45(hi_l2_94, wht_11, anf_970);
        return paint_45(hi_r2_95, wht_11, anf_971);
    }
    const vec3 pink_dk_9 = vec3(0.75, 0.42, 0.42);
    float smax_40(float a_41, float b_42, float k_43) {
        float anf_972 = (a_41 - b_42);
        float anf_973 = abs(anf_972);
        float anf_974 = (k_43 - anf_973);
        float anf_975 = max(anf_974, 0.);
        float h_44 = (anf_975 / k_43);
        float anf_976 = max(a_41, b_42);
        float anf_977 = (h_44 * h_44);
        float anf_978 = (anf_977 * k_43);
        float anf_979 = (anf_978 * 0.25);
        return (anf_976 + anf_979);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }


    ====== COMPILING EXAMPLE checkerboard.glml ======

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    const int size_0 = 5;
    uniform vec2 u_resolution;
    vec2 get_uv_1_vec2_to_vec2_48(vec2 coord_2) {
        vec2 anf_52 = (2. * coord_2);
        vec2 top_3 = (anf_52 - u_resolution);
        float anf_53 = u_resolution[0];
        float anf_54 = u_resolution[1];
        float bot_4 = min(anf_53, anf_54);
        return (top_3 / bot_4);
    }
    uniform float u_time;
    vec3 main_pure(vec2 coord_5) {
        vec2 uv_6 = get_uv_1_vec2_to_vec2_48(coord_5);
        float pf_65 = float(size_0);
        vec2 anf_55 = (uv_6 * pf_65);
        float anf_56 = (2. * u_time);
        vec2 anf_57 = vec2(anf_56, 0.);
        vec2 anf_58 = (anf_55 + anf_57);
        vec2 c_7 = floor(anf_58);
        float anf_59 = c_7[0];
        float anf_60 = c_7[1];
        float checker_sum_8 = (anf_59 + anf_60);
        float anf_61 = (checker_sum_8 / 2.);
        float anf_62 = floor(anf_61);
        float anf_63 = (anf_62 * 2.);
        float is_even_9 = (checker_sum_8 - anf_63);
        bool anf_64 = (is_even_9 < 0.5);
        if (anf_64) {
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
    uniform vec2 u_resolution;
    uniform float u_time;
    struct v_option_float {
        int tag;
        float Some_0;
    };
    v_option_float mandel_2_108(vec2 c_1, vec2 z_3, int i_4) {
        int _iter_143 = 0;
        while ((_iter_143 < 1000)) {
            bool anf_109 = (i_4 > 150);
            if (anf_109) {
                return v_option_float(1, 0.);
            } else {
                float anf_110 = length(z_3);
                bool anf_111 = (anf_110 > 4.);
                if (anf_111) {
                    float anf_112 = length(z_3);
                    float anf_113 = log2(anf_112);
                    float nu_5 = log2(anf_113);
                    float pf_146 = float(i_4);
                    float anf_114 = (pf_146 - nu_5);
                    float anf_115 = (anf_114 / 150.);
                    return v_option_float(0, anf_115);
                } else {
                    float anf_116 = z_3[0];
                    float anf_117 = z_3[0];
                    float anf_118 = (anf_116 * anf_117);
                    float anf_119 = z_3[1];
                    float anf_120 = z_3[1];
                    float anf_121 = (anf_119 * anf_120);
                    float zx_6 = (anf_118 - anf_121);
                    float anf_122 = z_3[0];
                    float anf_123 = (2. * anf_122);
                    float anf_124 = z_3[1];
                    float zy_7 = (anf_123 * anf_124);
                    vec2 anf_125 = vec2(zx_6, zy_7);
                    vec2 z_prime_8 = (anf_125 + c_1);
                    int anf_126 = (i_4 + 1);
                    c_1 = c_1;
                    z_3 = z_prime_8;
                    i_4 = anf_126;
                    int _iter_inc_144 = (_iter_143 + 1);
                    _iter_143 = _iter_inc_144;
                    continue;
                }
            }
        }
        v_option_float _tmp_147;
        return _tmp_147;
    }
    v_option_float mandelbrot_0_vec2_to_v_option_float_106(vec2 c_1) {
        vec2 anf_127 = vec2(0., 0.);
        return mandel_2_108(c_1, anf_127, 0);
    }
    vec3 main_pure(vec2 coord_9) {
        vec2 anf_128 = (2. * coord_9);
        vec2 top_11 = (anf_128 - u_resolution);
        float anf_129 = u_resolution[0];
        float anf_130 = u_resolution[1];
        float bot_12 = min(anf_129, anf_130);
        vec2 uv_10 = (top_11 / bot_12);
        float anf_131 = (u_time * 0.4);
        float anf_132 = sin(anf_131);
        float anf_133 = (anf_132 * 4.5);
        float anf_134 = (anf_133 + 3.5);
        float zoom_13 = exp(anf_134);
        vec2 anf_135 = vec2(-0.7453, 0.1127);
        vec2 anf_136 = (uv_10 / zoom_13);
        vec2 seahorse_valley_14 = (anf_135 + anf_136);
        v_option_float anf_137 = mandelbrot_0_vec2_to_v_option_float_106(seahorse_valley_14);
        int _lv_tag_145 = anf_137.tag;
        switch (_lv_tag_145) {
            case 1: {
                return vec3(0., 0., 0.);
                break;
            }
            default: {
                float n_15 = anf_137.Some_0;
                vec3 anf_138 = vec3(10., 20., 30.);
                vec3 anf_139 = (n_15 * anf_138);
                vec3 anf_140 = (anf_139 + u_time);
                vec3 anf_141 = sin(anf_140);
                vec3 anf_142 = (anf_141 * 0.5);
                return (anf_142 + 0.5);
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
    vec2 get_uv_0_vec2_to_vec2_42(vec2 coord_1) {
        vec2 anf_46 = (2. * coord_1);
        vec2 top_2 = (anf_46 - u_resolution);
        float anf_47 = u_resolution[0];
        float anf_48 = u_resolution[1];
        float bot_3 = min(anf_47, anf_48);
        return (top_2 / bot_3);
    }
    uniform float u_time;
    vec3 main_pure(vec2 coord_4) {
        vec2 uv_5 = get_uv_0_vec2_to_vec2_42(coord_4);
        vec2 anf_49 = (2. * u_mouse);
        vec2 anf_50 = (anf_49 - u_resolution);
        float anf_51 = u_resolution[1];
        vec2 mouseUV_6 = (anf_50 / anf_51);
        float anf_52 = (u_time * 2.);
        float anf_53 = sin(anf_52);
        float anf_54 = (anf_53 * 0.1);
        float radius_7 = (anf_54 + 0.15);
        float anf_55 = distance(uv_5, mouseUV_6);
        bool anf_56 = (anf_55 < radius_7);
        if (anf_56) {
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
    const vec3 deepColor_53 = vec3(0.02, 0.05, 0.2);
    float hash_10_431(vec3 p_11) {
        vec3 anf_435 = vec3(127.1, 311.7, 74.7);
        float d_12 = dot(p_11, anf_435);
        float anf_436 = sin(d_12);
        float anf_437 = (anf_436 * 43758.5453);
        return fract(anf_437);
    }
    const vec3 landColor_54 = vec3(0.15, 0.35, 0.1);
    const vec3 mountColor_55 = vec3(0.4, 0.3, 0.2);
    float noise3d_5(vec3 p_6) {
        vec3 i_7 = floor(p_6);
        vec3 f_8 = fract(p_6);
        vec3 anf_438 = (f_8 * f_8);
        vec3 anf_439 = (2. * f_8);
        vec3 anf_440 = (3. - anf_439);
        vec3 u_9 = (anf_438 * anf_440);
        float a_13 = hash_10_431(i_7);
        vec3 anf_441 = vec3(1., 0., 0.);
        vec3 anf_442 = (i_7 + anf_441);
        float b_14 = hash_10_431(anf_442);
        vec3 anf_443 = vec3(0., 1., 0.);
        vec3 anf_444 = (i_7 + anf_443);
        float c_15 = hash_10_431(anf_444);
        vec3 anf_445 = vec3(1., 1., 0.);
        vec3 anf_446 = (i_7 + anf_445);
        float d_16 = hash_10_431(anf_446);
        vec3 anf_447 = vec3(0., 0., 1.);
        vec3 anf_448 = (i_7 + anf_447);
        float e_17 = hash_10_431(anf_448);
        vec3 anf_449 = vec3(1., 0., 1.);
        vec3 anf_450 = (i_7 + anf_449);
        float f_18 = hash_10_431(anf_450);
        vec3 anf_451 = vec3(0., 1., 1.);
        vec3 anf_452 = (i_7 + anf_451);
        float g_19 = hash_10_431(anf_452);
        vec3 anf_453 = vec3(1., 1., 1.);
        vec3 anf_454 = (i_7 + anf_453);
        float h_20 = hash_10_431(anf_454);
        float anf_455 = u_9[0];
        float ab_21 = mix(a_13, b_14, anf_455);
        float anf_456 = u_9[0];
        float cd_22 = mix(c_15, d_16, anf_456);
        float anf_457 = u_9[0];
        float ef_23 = mix(e_17, f_18, anf_457);
        float anf_458 = u_9[0];
        float gh_24 = mix(g_19, h_20, anf_458);
        float anf_459 = u_9[1];
        float abcd_25 = mix(ab_21, cd_22, anf_459);
        float anf_460 = u_9[1];
        float efgh_26 = mix(ef_23, gh_24, anf_460);
        float anf_461 = u_9[2];
        return mix(abcd_25, efgh_26, anf_461);
    }
    float fbm_27(vec3 p_28) {
        vec3 anf_462 = (p_28 * 1.);
        float anf_463 = noise3d_5(anf_462);
        float anf_464 = (anf_463 * 0.5);
        vec3 anf_465 = (p_28 * 2.);
        float anf_466 = noise3d_5(anf_465);
        float anf_467 = (anf_466 * 0.25);
        float anf_468 = (anf_464 + anf_467);
        vec3 anf_469 = (p_28 * 4.);
        float anf_470 = noise3d_5(anf_469);
        float anf_471 = (anf_470 * 0.125);
        float anf_472 = (anf_468 + anf_471);
        vec3 anf_473 = (p_28 * 8.);
        float anf_474 = noise3d_5(anf_473);
        float anf_475 = (anf_474 * 0.0625);
        float anf_476 = (anf_472 + anf_475);
        vec3 anf_477 = (p_28 * 16.);
        float anf_478 = noise3d_5(anf_477);
        float anf_479 = (anf_478 * 0.03125);
        return (anf_476 + anf_479);
    }
    vec2 rotate_0(vec2 p_1, float angle_2) {
        float s_3 = sin(angle_2);
        float c_4 = cos(angle_2);
        float anf_480 = p_1[0];
        float anf_481 = (anf_480 * c_4);
        float anf_482 = p_1[1];
        float anf_483 = (anf_482 * s_3);
        float anf_484 = (anf_481 - anf_483);
        float anf_485 = p_1[0];
        float anf_486 = (anf_485 * s_3);
        float anf_487 = p_1[1];
        float anf_488 = (anf_487 * c_4);
        float anf_489 = (anf_486 + anf_488);
        return vec2(anf_484, anf_489);
    }
    vec3 rotate_by_mouse_61_vec3_int_to_vec3_430_434(vec2 mouseUV_60, vec3 ray_62) {
        float anf_490 = mouseUV_60[1];
        float anf_491 = (-1. * anf_490);
        float rotX_63 = (anf_491 * 1.5);
        float anf_492 = ray_62[1];
        float anf_493 = ray_62[2];
        vec2 anf_494 = vec2(anf_492, anf_493);
        vec2 ro_yz_64 = rotate_0(anf_494, rotX_63);
        float anf_495 = mouseUV_60[0];
        float anf_496 = (-1. * anf_495);
        float rotY_65 = (anf_496 * 1.5);
        float anf_497 = ray_62[0];
        float anf_498 = ro_yz_64[1];
        vec2 anf_499 = vec2(anf_497, anf_498);
        vec2 ro_xz_66 = rotate_0(anf_499, rotY_65);
        float anf_500 = ro_xz_66[0];
        float anf_501 = ro_yz_64[0];
        float anf_502 = ro_xz_66[1];
        return vec3(anf_500, anf_501, anf_502);
    }
    vec3 rotate_by_mouse_61_vec3_to_vec3_429_433(vec2 mouseUV_60, vec3 ray_62) {
        float anf_503 = mouseUV_60[1];
        float anf_504 = (-1. * anf_503);
        float rotX_63 = (anf_504 * 1.5);
        float anf_505 = ray_62[1];
        float anf_506 = ray_62[2];
        vec2 anf_507 = vec2(anf_505, anf_506);
        vec2 ro_yz_64 = rotate_0(anf_507, rotX_63);
        float anf_508 = mouseUV_60[0];
        float anf_509 = (-1. * anf_508);
        float rotY_65 = (anf_509 * 1.5);
        float anf_510 = ray_62[0];
        float anf_511 = ro_yz_64[1];
        vec2 anf_512 = vec2(anf_510, anf_511);
        vec2 ro_xz_66 = rotate_0(anf_512, rotY_65);
        float anf_513 = ro_xz_66[0];
        float anf_514 = ro_yz_64[0];
        float anf_515 = ro_xz_66[1];
        return vec3(anf_513, anf_514, anf_515);
    }
    float sdPlanet_29(vec3 p_30, float radius_31) {
        float len_32 = length(p_30);
        vec3 dir_33 = (p_30 / len_32);
        vec3 anf_516 = (dir_33 * 3.);
        float anf_517 = fbm_27(anf_516);
        float terrain_34 = (anf_517 * 0.4);
        float anf_518 = (len_32 - radius_31);
        return (anf_518 - terrain_34);
    }
    float map_35(vec3 p_36) {
        return sdPlanet_29(p_36, 1.5);
    }
    vec3 getNormal_37(vec3 p_38) {
        float e_39 = 0.002;
        vec3 e_x_40 = vec3(e_39, 0., 0.);
        vec3 e_y_41 = vec3(0., e_39, 0.);
        vec3 e_z_42 = vec3(0., 0., e_39);
        vec3 anf_519 = (p_38 + e_x_40);
        float anf_520 = map_35(anf_519);
        vec3 anf_521 = (p_38 - e_x_40);
        float anf_522 = map_35(anf_521);
        float dx_43 = (anf_520 - anf_522);
        vec3 anf_523 = (p_38 + e_y_41);
        float anf_524 = map_35(anf_523);
        vec3 anf_525 = (p_38 - e_y_41);
        float anf_526 = map_35(anf_525);
        float dy_44 = (anf_524 - anf_526);
        vec3 anf_527 = (p_38 + e_z_42);
        float anf_528 = map_35(anf_527);
        vec3 anf_529 = (p_38 - e_z_42);
        float anf_530 = map_35(anf_529);
        float dz_45 = (anf_528 - anf_530);
        vec3 anf_531 = vec3(dx_43, dy_44, dz_45);
        return normalize(anf_531);
    }
    const vec3 snowColor_56 = vec3(0.85, 0.85, 0.9);
    uniform vec2 u_mouse;
    uniform vec2 u_resolution;
    struct v_option_float {
        int tag;
        float Some_0;
    };
    v_option_float march_49_432(vec3 rd_48, vec3 ro_47, float t_50, int steps_51) {
        int _iter_575 = 0;
        while ((_iter_575 < 1000)) {
            bool anf_532 = (steps_51 > 120);
            if (anf_532) {
                return v_option_float(1, 0.);
            } else {
                vec3 anf_533 = (rd_48 * t_50);
                vec3 anf_534 = (ro_47 + anf_533);
                float d_52 = map_35(anf_534);
                bool anf_535 = (d_52 < 0.0005);
                if (anf_535) {
                    return v_option_float(0, t_50);
                } else {
                    bool anf_536 = (t_50 > 50.);
                    if (anf_536) {
                        return v_option_float(1, 0.);
                    } else {
                        float anf_537 = (d_52 * 0.8);
                        float anf_538 = (t_50 + anf_537);
                        int anf_539 = (steps_51 + 1);
                        rd_48 = rd_48;
                        ro_47 = ro_47;
                        t_50 = anf_538;
                        steps_51 = anf_539;
                        int _iter_inc_576 = (_iter_575 + 1);
                        _iter_575 = _iter_inc_576;
                        continue;
                    }
                }
            }
        }
        v_option_float _tmp_578;
        return _tmp_578;
    }
    v_option_float march_46(vec3 ro_47, vec3 rd_48) {
        return march_49_432(rd_48, ro_47, 0., 0);
    }
    vec3 main_pure(vec2 coord_57) {
        float anf_540 = u_resolution[0];
        float anf_541 = u_resolution[1];
        float res_min_58 = min(anf_540, anf_541);
        vec2 anf_542 = (coord_57 * 2.);
        vec2 anf_543 = (anf_542 - u_resolution);
        vec2 uv_59 = (anf_543 / res_min_58);
        vec2 anf_544 = (u_mouse * 2.);
        vec2 anf_545 = (anf_544 - u_resolution);
        vec2 mouseUV_60 = (anf_545 / res_min_58);
        vec3 anf_546 = vec3(0., 0., -4.);
        vec3 ro_67 = rotate_by_mouse_61_vec3_int_to_vec3_430_434(mouseUV_60, anf_546);
        float anf_547 = uv_59[0];
        float anf_548 = uv_59[1];
        vec3 anf_549 = vec3(anf_547, anf_548, 1.5);
        vec3 anf_550 = normalize(anf_549);
        vec3 rd_68 = rotate_by_mouse_61_vec3_to_vec3_429_433(mouseUV_60, anf_550);
        v_option_float t_69 = march_46(ro_67, rd_68);
        int _lv_tag_577 = t_69.tag;
        switch (_lv_tag_577) {
            case 1: {
                return vec3(0., 0., 0.);
                break;
            }
            default: {
                float t_70 = t_69.Some_0;
                vec3 anf_551 = (rd_68 * t_70);
                vec3 hitPos_71 = (ro_67 + anf_551);
                vec3 n_72 = getNormal_37(hitPos_71);
                vec3 anf_552 = vec3(1., 0.8, -0.5);
                vec3 lightDir_73 = normalize(anf_552);
                float anf_553 = dot(n_72, lightDir_73);
                float diff_74 = max(anf_553, 0.);
                float ambient_75 = 0.08;
                float anf_554 = length(hitPos_71);
                vec3 dir_76 = (hitPos_71 / anf_554);
                vec3 anf_555 = (dir_76 * 3.);
                float rawHeight_77 = fbm_27(anf_555);
                float seaLevel_78 = 0.35;
                float anf_556 = (rawHeight_77 - seaLevel_78);
                float anf_557 = (1. - seaLevel_78);
                float anf_558 = (anf_556 / anf_557);
                float h_norm_79 = clamp(anf_558, 0., 1.);
                bool anf_559 = (h_norm_79 < 0.3);
                vec3 baseColor_80;
                if (anf_559) {
                    float anf_560 = (h_norm_79 / 0.3);
                    baseColor_80 = mix(deepColor_53, landColor_54, anf_560);
                } else {
                    bool anf_561 = (h_norm_79 < 0.6);
                    if (anf_561) {
                        float anf_562 = (h_norm_79 - 0.3);
                        float anf_563 = (anf_562 / 0.3);
                        baseColor_80 = mix(landColor_54, mountColor_55, anf_563);
                    } else {
                        float anf_564 = (h_norm_79 - 0.6);
                        float anf_565 = (anf_564 / 0.4);
                        baseColor_80 = mix(mountColor_55, snowColor_56, anf_565);
                    }
                }
                vec3 anf_566 = (rd_68 * -1.);
                float anf_567 = dot(n_72, anf_566);
                float anf_568 = max(anf_567, 0.);
                float fresnel_81 = (1. - anf_568);
                float anf_569 = (fresnel_81 * fresnel_81);
                float anf_570 = (anf_569 * fresnel_81);
                float rim_82 = (anf_570 * 0.4);
                vec3 atmoColor_83 = vec3(0.3, 0.5, 1.);
                float anf_571 = (diff_74 * 0.9);
                float anf_572 = (anf_571 + ambient_75);
                vec3 anf_573 = (baseColor_80 * anf_572);
                vec3 anf_574 = (atmoColor_83 * rim_82);
                return (anf_573 + anf_574);
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
    vec2 get_uv_0_vec2_to_vec2_39(vec2 coord_1) {
        vec2 anf_43 = (2. * coord_1);
        vec2 top_2 = (anf_43 - u_resolution);
        float anf_44 = u_resolution[0];
        float anf_45 = u_resolution[1];
        float bot_3 = min(anf_44, anf_45);
        return (top_2 / bot_3);
    }
    uniform float u_time;
    vec3 main_pure(vec2 coord_4) {
        vec2 uv_5 = get_uv_0_vec2_to_vec2_39(coord_4);
        float anf_46 = uv_5[0];
        float anf_47 = uv_5[1];
        float anf_48 = (anf_46 + anf_47);
        float anf_49 = (5. * anf_48);
        float wave_6 = (anf_49 + u_time);
        vec3 anf_50 = vec3(0., 2., 4.);
        vec3 anf_51 = (wave_6 + anf_50);
        vec3 anf_52 = sin(anf_51);
        vec3 anf_53 = (anf_52 * 0.3);
        return (anf_53 + 0.7);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }


    ====== COMPILING EXAMPLE raymarch.glml ======

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    vec3 palette_10(float t_11) {
        vec3 cfg_12 = vec3(0.3, 0.416, 0.557);
        vec3 anf_259 = (cfg_12 + t_11);
        vec3 anf_260 = (anf_259 * 6.28318);
        vec3 anf_261 = cos(anf_260);
        vec3 anf_262 = (anf_261 * 0.5);
        return (anf_262 + 0.5);
    }
    vec2 rotate_0(vec2 p_1, float angle_2) {
        float s_3 = sin(angle_2);
        float c_4 = cos(angle_2);
        float anf_263 = p_1[0];
        float anf_264 = (anf_263 * c_4);
        float anf_265 = p_1[1];
        float anf_266 = (anf_265 * s_3);
        float anf_267 = (anf_264 - anf_266);
        float anf_268 = p_1[0];
        float anf_269 = (anf_268 * s_3);
        float anf_270 = p_1[1];
        float anf_271 = (anf_270 * c_4);
        float anf_272 = (anf_269 + anf_271);
        return vec2(anf_267, anf_272);
    }
    float sMin_5(float a_6, float b_7) {
        float k_8 = 0.1;
        float anf_273 = (b_7 - a_6);
        float anf_274 = (0.5 * anf_273);
        float anf_275 = (anf_274 / k_8);
        float anf_276 = (0.5 + anf_275);
        float h_9 = clamp(anf_276, 0., 1.);
        float anf_277 = mix(b_7, a_6, h_9);
        float anf_278 = (k_8 * h_9);
        float anf_279 = (1. - h_9);
        float anf_280 = (anf_278 * anf_279);
        return (anf_277 - anf_280);
    }
    float sdTorus_13(vec3 p_14, vec2 t_15) {
        float anf_281 = p_14[0];
        float anf_282 = p_14[2];
        vec2 anf_283 = vec2(anf_281, anf_282);
        float anf_284 = length(anf_283);
        float anf_285 = t_15[0];
        float anf_286 = (anf_284 - anf_285);
        float anf_287 = p_14[1];
        vec2 q_16 = vec2(anf_286, anf_287);
        float anf_288 = length(q_16);
        float anf_289 = t_15[1];
        return (anf_288 - anf_289);
    }
    uniform vec2 u_mouse;
    uniform vec2 u_resolution;
    uniform float u_time;
    float map_17(vec3 p_18) {
        float angle_19 = (u_time * 2.);
        float anf_290 = p_18[0];
        float anf_291 = p_18[1];
        vec2 anf_292 = vec2(anf_290, anf_291);
        vec2 p_xy_20 = rotate_0(anf_292, angle_19);
        float anf_293 = p_xy_20[0];
        float anf_294 = p_xy_20[1];
        float anf_295 = p_18[2];
        vec3 p_prime_21 = vec3(anf_293, anf_294, anf_295);
        float anf_296 = p_prime_21[1];
        float anf_297 = p_prime_21[2];
        vec2 anf_298 = vec2(anf_296, anf_297);
        vec2 p_yz_22 = rotate_0(anf_298, angle_19);
        float anf_299 = p_prime_21[0];
        float anf_300 = p_yz_22[0];
        float anf_301 = p_yz_22[1];
        vec3 p_prime_23 = vec3(anf_299, anf_300, anf_301);
        vec2 anf_302 = vec2(1., 0.3);
        float anf_303 = sdTorus_13(p_prime_23, anf_302);
        vec2 anf_304 = vec2(2., 0.5);
        float anf_305 = sdTorus_13(p_18, anf_304);
        return sMin_5(anf_303, anf_305);
    }
    struct v_option_float {
        int tag;
        float Some_0;
    };
    v_option_float march_27_258(vec3 rd_26, vec3 ro_25, float t_28, int steps_29) {
        int _iter_352 = 0;
        while ((_iter_352 < 1000)) {
            bool anf_306 = (steps_29 > 80);
            if (anf_306) {
                return v_option_float(1, 0.);
            } else {
                vec3 anf_307 = (rd_26 * t_28);
                vec3 anf_308 = (ro_25 + anf_307);
                float d_30 = map_17(anf_308);
                bool anf_309 = (d_30 < 0.001);
                if (anf_309) {
                    return v_option_float(0, t_28);
                } else {
                    bool anf_310 = (t_28 > 100.);
                    if (anf_310) {
                        return v_option_float(1, 0.);
                    } else {
                        float anf_311 = (t_28 + d_30);
                        int anf_312 = (steps_29 + 1);
                        rd_26 = rd_26;
                        ro_25 = ro_25;
                        t_28 = anf_311;
                        steps_29 = anf_312;
                        int _iter_inc_353 = (_iter_352 + 1);
                        _iter_352 = _iter_inc_353;
                        continue;
                    }
                }
            }
        }
        v_option_float _tmp_355;
        return _tmp_355;
    }
    v_option_float march_24(vec3 ro_25, vec3 rd_26) {
        return march_27_258(rd_26, ro_25, 0., 0);
    }
    vec3 main_pure(vec2 coord_31) {
        float anf_313 = u_resolution[0];
        float anf_314 = u_resolution[1];
        float res_min_32 = min(anf_313, anf_314);
        vec2 anf_315 = (coord_31 * 2.);
        vec2 anf_316 = (anf_315 - u_resolution);
        vec2 uv_33 = (anf_316 / res_min_32);
        vec2 anf_317 = (u_mouse * 2.);
        vec2 anf_318 = (anf_317 - u_resolution);
        vec2 mouseUV_34 = (anf_318 / res_min_32);
        vec3 ro_init_35 = vec3(0., 0., -6.);
        float anf_319 = uv_33[0];
        float anf_320 = uv_33[1];
        vec3 anf_321 = vec3(anf_319, anf_320, 1.);
        vec3 rd_init_36 = normalize(anf_321);
        float anf_322 = mouseUV_34[1];
        float rotX_37 = (-1. * anf_322);
        float anf_323 = mouseUV_34[0];
        float rotY_38 = (-1. * anf_323);
        float anf_324 = ro_init_35[1];
        float anf_325 = ro_init_35[2];
        vec2 anf_326 = vec2(anf_324, anf_325);
        vec2 ro_yz_39 = rotate_0(anf_326, rotX_37);
        float anf_327 = rd_init_36[1];
        float anf_328 = rd_init_36[2];
        vec2 anf_329 = vec2(anf_327, anf_328);
        vec2 rd_yz_40 = rotate_0(anf_329, rotX_37);
        float anf_330 = ro_init_35[0];
        float anf_331 = ro_yz_39[0];
        float anf_332 = ro_yz_39[1];
        vec3 ro_41 = vec3(anf_330, anf_331, anf_332);
        float anf_333 = rd_init_36[0];
        float anf_334 = rd_yz_40[0];
        float anf_335 = rd_yz_40[1];
        vec3 rd_42 = vec3(anf_333, anf_334, anf_335);
        float anf_336 = ro_41[0];
        float anf_337 = ro_41[2];
        vec2 anf_338 = vec2(anf_336, anf_337);
        vec2 ro_xz_43 = rotate_0(anf_338, rotY_38);
        float anf_339 = rd_42[0];
        float anf_340 = rd_42[2];
        vec2 anf_341 = vec2(anf_339, anf_340);
        vec2 rd_xz_44 = rotate_0(anf_341, rotY_38);
        float anf_342 = ro_xz_43[0];
        float anf_343 = ro_41[1];
        float anf_344 = ro_xz_43[1];
        vec3 ro_45 = vec3(anf_342, anf_343, anf_344);
        float anf_345 = rd_xz_44[0];
        float anf_346 = rd_42[1];
        float anf_347 = rd_xz_44[1];
        vec3 rd_46 = vec3(anf_345, anf_346, anf_347);
        v_option_float anf_348 = march_24(ro_45, rd_46);
        int _lv_tag_354 = anf_348.tag;
        vec3 col_47;
        switch (_lv_tag_354) {
            case 1: {
                col_47 = vec3(0.2, 0.2, 0.2);
                break;
            }
            default: {
                float t_48 = anf_348.Some_0;
                float anf_349 = (t_48 * 0.3);
                col_47 = palette_10(anf_349);
                break;
            }
        }
        vec2 anf_350 = (uv_33 - mouseUV_34);
        float anf_351 = length(anf_350);
        float glow_49 = (0.02 / anf_351);
        return (col_47 + glow_49);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }


    ====== COMPILING EXAMPLE recursion.glml ======

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float gcd_8_float_to_float_to_float_80(float a_9, float b_10) {
        int _iter_101 = 0;
        while ((_iter_101 < 1000)) {
            bool anf_81 = (a_9 < 0.05);
            if (anf_81) {
                return b_10;
            } else {
                bool anf_82 = (b_10 < 0.05);
                if (anf_82) {
                    return a_9;
                } else {
                    bool anf_83 = (a_9 > b_10);
                    if (anf_83) {
                        float anf_84 = (a_9 - b_10);
                        a_9 = anf_84;
                        b_10 = b_10;
                        int _iter_inc_102 = (_iter_101 + 1);
                        _iter_101 = _iter_inc_102;
                        continue;
                    } else {
                        float anf_85 = (b_10 - a_9);
                        a_9 = a_9;
                        b_10 = anf_85;
                        int _iter_inc_103 = (_iter_101 + 1);
                        _iter_101 = _iter_inc_103;
                        continue;
                    }
                }
            }
        }
        return 0.;
    }
    mat2 rotate_4(float angle_5) {
        float s_6 = sin(angle_5);
        float c_7 = cos(angle_5);
        float anf_86 = (-1. * s_6);
        vec2 anf_87 = vec2(c_7, anf_86);
        vec2 anf_88 = vec2(s_6, c_7);
        return mat2(anf_87, anf_88);
    }
    uniform vec2 u_resolution;
    vec2 get_uv_0(vec2 coord_1) {
        vec2 anf_89 = (2. * coord_1);
        vec2 top_2 = (anf_89 - u_resolution);
        float anf_90 = u_resolution[0];
        float anf_91 = u_resolution[1];
        float bot_3 = min(anf_90, anf_91);
        return (top_2 / bot_3);
    }
    uniform float u_time;
    vec3 main_pure(vec2 coord_11) {
        mat2 anf_92 = rotate_4(u_time);
        vec2 anf_93 = get_uv_0(coord_11);
        vec2 uv_12 = (anf_92 * anf_93);
        float anf_94 = (u_time * 2.);
        float anf_95 = sin(anf_94);
        vec2 anf_96 = (uv_12 * anf_95);
        vec2 anf_97 = (anf_96 * 2.);
        vec2 anf_98 = abs(anf_97);
        float x_13 = anf_98[0];
        float y_14 = anf_98[1];
        float res_15 = gcd_8_float_to_float_to_float_80(x_13, y_14);
        float anf_99 = (res_15 * 0.5);
        float anf_100 = (1. - res_15);
        return vec3(res_15, anf_99, anf_100);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }


    ====== COMPILING EXAMPLE warped_noise.glml ======

    #version 300 es
    precision highp float;
    out vec4 fragColor;
    float smoothNoise_0(vec2 p_1) {
        vec2 i_2 = floor(p_1);
        vec2 pf_3 = (p_1 - i_2);
        vec2 anf_245 = (pf_3 * pf_3);
        vec2 anf_246 = (2. * pf_3);
        vec2 anf_247 = (3. - anf_246);
        vec2 inter_4 = (anf_245 * anf_247);
        vec4 v4_5 = vec4(0., 1., 27., 28.);
        float anf_248 = i_2[0];
        vec4 anf_249 = (v4_5 + anf_248);
        float anf_250 = i_2[1];
        float anf_251 = (anf_250 * 27.);
        vec4 seed_6 = (anf_249 + anf_251);
        vec4 anf_252 = mod(seed_6, 6.2831853);
        vec4 anf_253 = sin(anf_252);
        vec4 anf_254 = (anf_253 * 200000.);
        vec4 hash_7 = fract(anf_254);
        float anf_255 = hash_7[0];
        float anf_256 = hash_7[1];
        vec2 col0_8 = vec2(anf_255, anf_256);
        float anf_257 = hash_7[2];
        float anf_258 = hash_7[3];
        vec2 col1_9 = vec2(anf_257, anf_258);
        float anf_259 = inter_4[1];
        float anf_260 = (1. - anf_259);
        vec2 anf_261 = (col0_8 * anf_260);
        float anf_262 = inter_4[1];
        vec2 anf_263 = (col1_9 * anf_262);
        vec2 res_v_10 = (anf_261 + anf_263);
        float anf_264 = inter_4[0];
        float anf_265 = (1. - anf_264);
        float anf_266 = inter_4[0];
        vec2 anf_267 = vec2(anf_265, anf_266);
        return dot(res_v_10, anf_267);
    }
    float fractalNoise_11(vec2 p_12) {
        float anf_268 = smoothNoise_0(p_12);
        float anf_269 = (anf_268 * 0.5333);
        vec2 anf_270 = (p_12 * 2.);
        float anf_271 = smoothNoise_0(anf_270);
        float anf_272 = (anf_271 * 0.2667);
        float anf_273 = (anf_269 + anf_272);
        vec2 anf_274 = (p_12 * 4.);
        float anf_275 = smoothNoise_0(anf_274);
        float anf_276 = (anf_275 * 0.1333);
        float anf_277 = (anf_273 + anf_276);
        vec2 anf_278 = (p_12 * 8.);
        float anf_279 = smoothNoise_0(anf_278);
        float anf_280 = (anf_279 * 0.0667);
        return (anf_277 + anf_280);
    }
    uniform vec2 u_resolution;
    uniform float u_time;
    float warpedNoise_13(vec2 p_14) {
        float anf_281 = (-1. * u_time);
        vec2 anf_282 = vec2(u_time, anf_281);
        vec2 m_15 = (anf_282 * 0.5);
        vec2 anf_283 = (p_14 + m_15);
        float x_16 = fractalNoise_11(anf_283);
        float anf_284 = m_15[1];
        float anf_285 = m_15[0];
        vec2 anf_286 = vec2(anf_284, anf_285);
        vec2 anf_287 = (p_14 + anf_286);
        vec2 anf_288 = (anf_287 + x_16);
        float y_17 = fractalNoise_11(anf_288);
        vec2 anf_289 = (p_14 - m_15);
        vec2 anf_290 = (anf_289 - x_16);
        vec2 anf_291 = (anf_290 + y_17);
        float z_18 = fractalNoise_11(anf_291);
        vec2 anf_292 = vec2(x_16, y_17);
        vec2 anf_293 = vec2(y_17, z_18);
        vec2 anf_294 = (anf_292 + anf_293);
        vec2 anf_295 = vec2(z_18, x_16);
        vec2 warp_19 = (anf_294 + anf_295);
        vec3 anf_296 = vec3(x_16, y_17, z_18);
        float anf_297 = length(anf_296);
        float mag_20 = (anf_297 * 0.25);
        vec2 anf_298 = (p_14 + warp_19);
        vec2 anf_299 = (anf_298 + mag_20);
        return fractalNoise_11(anf_299);
    }
    vec3 main_pure(vec2 coord_21) {
        vec2 anf_300 = (u_resolution * 0.5);
        vec2 anf_301 = (coord_21 - anf_300);
        float anf_302 = u_resolution[1];
        vec2 uv_22 = (anf_301 / anf_302);
        vec2 anf_303 = (uv_22 * 6.);
        float n_23 = warpedNoise_13(anf_303);
        vec2 anf_304 = (uv_22 * 6.);
        vec2 anf_305 = (anf_304 - 0.02);
        float n2_24 = warpedNoise_13(anf_305);
        float anf_306 = (n2_24 - n_23);
        float anf_307 = max(anf_306, 0.);
        float anf_308 = (anf_307 / 0.02);
        float bump_25 = (anf_308 * 0.7071);
        float anf_309 = (n_23 - n2_24);
        float anf_310 = max(anf_309, 0.);
        float anf_311 = (anf_310 / 0.02);
        float bump2_26 = (anf_311 * 0.7071);
        float anf_312 = (bump_25 * bump_25);
        float anf_313 = pow(bump_25, 4.);
        float anf_314 = (anf_313 * 0.5);
        float b1_27 = (anf_312 + anf_314);
        float anf_315 = (bump2_26 * bump2_26);
        float anf_316 = pow(bump2_26, 4.);
        float anf_317 = (anf_316 * 0.5);
        float b2_28 = (anf_315 + anf_317);
        vec3 anf_318 = vec3(1., 0.7, 0.6);
        float anf_319 = (b1_27 + b2_28);
        float anf_320 = (anf_319 * 0.4);
        vec3 anf_321 = vec3(b1_27, anf_320, b2_28);
        vec3 anf_322 = (anf_318 * anf_321);
        vec3 anf_323 = (anf_322 * 0.3);
        vec3 base_col_29 = (anf_323 + 0.5);
        float anf_324 = (n_23 * n_23);
        vec3 col_30 = (anf_324 * base_col_29);
        vec2 anf_325 = (uv_22 - 0.65);
        float spot1_dist_31 = length(anf_325);
        vec2 anf_326 = (uv_22 + 0.5);
        float spot2_dist_32 = length(anf_326);
        vec3 anf_327 = vec3(0.8, 0.4, 1.);
        vec3 anf_328 = (anf_327 * 0.35);
        vec3 anf_329 = vec3(1., 0.5, 0.2);
        float anf_330 = (1. - spot1_dist_31);
        float anf_331 = smoothstep(0., 1., anf_330);
        vec3 anf_332 = (anf_329 * anf_331);
        vec3 anf_333 = vec3(0.2, 0.4, 1.);
        float anf_334 = (1. - spot2_dist_32);
        float anf_335 = smoothstep(0., 1., anf_334);
        vec3 anf_336 = (anf_333 * anf_335);
        vec3 anf_337 = (anf_332 + anf_336);
        vec3 anf_338 = (anf_337 * 5.);
        vec3 spot_logic_33 = (anf_328 + anf_338);
        vec3 final_col_34 = (col_30 * spot_logic_33);
        vec3 anf_339 = max(final_col_34, 0.);
        return sqrt(anf_339);
    }
    void main() {
        vec3 color = main_pure(gl_FragCoord.xy);
        fragColor = clamp(vec4(color.xyz, 1.), 0., 1.);
    }
    |}]
;;
