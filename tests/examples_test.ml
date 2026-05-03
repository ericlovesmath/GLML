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
        int _lv_tag_226 = s_1.tag;
        switch (_lv_tag_226) {
            case 0: {
                float r_3 = s_1.Circle_0;
                float anf_188 = length(p_2);
                return (anf_188 - r_3);
                break;
            }
            case 1: {
                float w_4 = s_1.Rect_0;
                float h_5 = s_1.Rect_1;
                vec2 anf_189 = abs(p_2);
                vec2 anf_190 = vec2(w_4, h_5);
                vec2 d_6 = (anf_189 - anf_190);
                vec2 anf_191 = vec2(0., 0.);
                vec2 anf_192 = max(d_6, anf_191);
                float anf_193 = length(anf_192);
                float anf_194 = d_6[0];
                float anf_195 = d_6[1];
                float anf_196 = max(anf_194, anf_195);
                float anf_197 = min(anf_196, 0.);
                return (anf_193 + anf_197);
                break;
            }
            default: {
                return 1.;
                break;
            }
        }
    }
    struct DFn_171 {
        int tag;
        shape lctor_174_0;
        shape lctor_177_0;
    };
    struct DFn_182 {
        int tag;
        DFn_171 lctor_183_0;
        DFn_171 lctor_183_1;
    };
    DFn_182 scene_11() {
        shape anf_198 = shape(0, 0.3, 0., 0.);
        shape _tmp_228;
        DFn_171 circle_12 = DFn_171(0, anf_198, _tmp_228);
        shape anf_199 = shape(1, 0., 0.7, 0.1);
        shape _tmp_229;
        DFn_171 rect_13 = DFn_171(1, _tmp_229, anf_199);
        return DFn_182(0, circle_12, rect_13);
    }
    float dapply_170(DFn_171 dfn_184, vec2 da_185) {
        int _lv_tag_227 = dfn_184.tag;
        switch (_lv_tag_227) {
            case 0: {
                shape ca_173 = dfn_184.lctor_174_0;
                return sdf_shape_0(ca_173, da_185);
                break;
            }
            default: {
                shape ca_176 = dfn_184.lctor_177_0;
                return sdf_shape_0(ca_176, da_185);
                break;
            }
        }
    }
    uniform vec2 u_mouse;
    uniform vec2 u_resolution;
    vec2 get_uv_14_vec2_to_vec2_166(vec2 coord_15) {
        vec2 anf_200 = (2. * coord_15);
        vec2 top_16 = (anf_200 - u_resolution);
        float anf_201 = u_resolution[0];
        float anf_202 = u_resolution[1];
        float bot_17 = min(anf_201, anf_202);
        return (top_16 / bot_17);
    }
    float union_7(DFn_171 f_8, DFn_171 f_prime_9, vec2 p_10) {
        float anf_203 = dapply_170(f_8, p_10);
        float anf_204 = dapply_170(f_prime_9, p_10);
        return min(anf_203, anf_204);
    }
    float dapply_181(DFn_182 dfn_186, vec2 da_187) {
        DFn_171 ca_179 = dfn_186.lctor_183_0;
        DFn_171 ca_180 = dfn_186.lctor_183_1;
        return union_7(ca_179, ca_180, da_187);
    }
    vec3 main_pure(vec2 coord_20) {
        vec2 p_21 = get_uv_14_vec2_to_vec2_166(coord_20);
        vec2 m_22 = get_uv_14_vec2_to_vec2_166(u_mouse);
        DFn_182 _lc_230 = scene_11();
        float d_23 = dapply_181(_lc_230, p_21);
        bool anf_205 = (d_23 > 0.);
        vec3 col_24;
        if (anf_205) {
            col_24 = orange_18;
        } else {
            col_24 = blue_19;
        }
        float anf_206 = abs(d_23);
        float anf_207 = (-6. * anf_206);
        float anf_208 = exp(anf_207);
        float darken_26 = (1. - anf_208);
        float anf_209 = (150. * d_23);
        float anf_210 = cos(anf_209);
        float anf_211 = (0.2 * anf_210);
        float rings_27 = (0.8 + anf_211);
        vec3 anf_212 = (col_24 * darken_26);
        vec3 col_25 = (anf_212 * rings_27);
        vec3 anf_213 = vec3(1., 1., 1.);
        float anf_214 = abs(d_23);
        float anf_215 = smoothstep(0., 0.01, anf_214);
        float anf_216 = (1. - anf_215);
        vec3 col_28 = mix(col_25, anf_213, anf_216);
        DFn_182 _lc_231 = scene_11();
        float anf_217 = dapply_181(_lc_231, m_22);
        float d_30 = abs(anf_217);
        vec2 anf_218 = (p_21 - m_22);
        float dm_31 = length(anf_218);
        float anf_219 = (dm_31 - d_30);
        float anf_220 = abs(anf_219);
        float anf_221 = (anf_220 - 0.0025);
        float anf_222 = (dm_31 - 0.015);
        float d_32 = min(anf_221, anf_222);
        vec3 anf_223 = vec3(1., 1., 0.);
        float anf_224 = smoothstep(0., 0.005, d_32);
        float anf_225 = (1. - anf_224);
        vec3 col_29 = mix(col_28, anf_223, anf_225);
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
    vec2 get_uv_1_vec2_to_vec2_49(vec2 coord_2) {
        vec2 anf_53 = (2. * coord_2);
        vec2 top_3 = (anf_53 - u_resolution);
        float anf_54 = u_resolution[0];
        float anf_55 = u_resolution[1];
        float bot_4 = min(anf_54, anf_55);
        return (top_3 / bot_4);
    }
    uniform float u_time;
    vec3 main_pure(vec2 coord_5) {
        vec2 uv_6 = get_uv_1_vec2_to_vec2_49(coord_5);
        float pf_66 = float(size_0);
        vec2 anf_56 = (uv_6 * pf_66);
        float anf_57 = (2. * u_time);
        vec2 anf_58 = vec2(anf_57, 0.);
        vec2 anf_59 = (anf_56 + anf_58);
        vec2 c_7 = floor(anf_59);
        float anf_60 = c_7[0];
        float anf_61 = c_7[1];
        float checker_sum_8 = (anf_60 + anf_61);
        float anf_62 = (checker_sum_8 / 2.);
        float anf_63 = floor(anf_62);
        float anf_64 = (anf_63 * 2.);
        float is_even_9 = (checker_sum_8 - anf_64);
        bool anf_65 = (is_even_9 < 0.5);
        if (anf_65) {
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
    v_option_float mandel_2_115(vec2 c_1, vec2 z_3, int i_4) {
        int _iter_150 = 0;
        while ((_iter_150 < 1000)) {
            bool anf_116 = (i_4 > 150);
            if (anf_116) {
                return v_option_float(1, 0.);
            } else {
                float anf_117 = length(z_3);
                bool anf_118 = (anf_117 > 4.);
                if (anf_118) {
                    float anf_119 = length(z_3);
                    float anf_120 = log2(anf_119);
                    float nu_5 = log2(anf_120);
                    float pf_153 = float(i_4);
                    float anf_121 = (pf_153 - nu_5);
                    float anf_122 = (anf_121 / 150.);
                    return v_option_float(0, anf_122);
                } else {
                    float anf_123 = z_3[0];
                    float anf_124 = z_3[0];
                    float anf_125 = (anf_123 * anf_124);
                    float anf_126 = z_3[1];
                    float anf_127 = z_3[1];
                    float anf_128 = (anf_126 * anf_127);
                    float zx_6 = (anf_125 - anf_128);
                    float anf_129 = z_3[0];
                    float anf_130 = (2. * anf_129);
                    float anf_131 = z_3[1];
                    float zy_7 = (anf_130 * anf_131);
                    vec2 anf_132 = vec2(zx_6, zy_7);
                    vec2 z_prime_8 = (anf_132 + c_1);
                    int anf_133 = (i_4 + 1);
                    c_1 = c_1;
                    z_3 = z_prime_8;
                    i_4 = anf_133;
                    int _iter_inc_151 = (_iter_150 + 1);
                    _iter_150 = _iter_inc_151;
                    continue;
                }
            }
        }
        v_option_float _tmp_154;
        return _tmp_154;
    }
    v_option_float mandelbrot_0_vec2_to_v_option_float_113(vec2 c_1) {
        vec2 anf_134 = vec2(0., 0.);
        return mandel_2_115(c_1, anf_134, 0);
    }
    vec3 main_pure(vec2 coord_9) {
        vec2 anf_135 = (2. * coord_9);
        vec2 top_11 = (anf_135 - u_resolution);
        float anf_136 = u_resolution[0];
        float anf_137 = u_resolution[1];
        float bot_12 = min(anf_136, anf_137);
        vec2 uv_10 = (top_11 / bot_12);
        float anf_138 = (u_time * 0.4);
        float anf_139 = sin(anf_138);
        float anf_140 = (anf_139 * 4.5);
        float anf_141 = (anf_140 + 3.5);
        float zoom_13 = exp(anf_141);
        vec2 anf_142 = vec2(-0.7453, 0.1127);
        vec2 anf_143 = (uv_10 / zoom_13);
        vec2 seahorse_valley_14 = (anf_142 + anf_143);
        v_option_float anf_144 = mandelbrot_0_vec2_to_v_option_float_113(seahorse_valley_14);
        int _lv_tag_152 = anf_144.tag;
        switch (_lv_tag_152) {
            case 1: {
                return vec3(0., 0., 0.);
                break;
            }
            default: {
                float n_15 = anf_144.Some_0;
                vec3 anf_145 = vec3(10., 20., 30.);
                vec3 anf_146 = (n_15 * anf_145);
                vec3 anf_147 = (anf_146 + u_time);
                vec3 anf_148 = sin(anf_147);
                vec3 anf_149 = (anf_148 * 0.5);
                return (anf_149 + 0.5);
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
    vec2 get_uv_0_vec2_to_vec2_43(vec2 coord_1) {
        vec2 anf_47 = (2. * coord_1);
        vec2 top_2 = (anf_47 - u_resolution);
        float anf_48 = u_resolution[0];
        float anf_49 = u_resolution[1];
        float bot_3 = min(anf_48, anf_49);
        return (top_2 / bot_3);
    }
    uniform float u_time;
    vec3 main_pure(vec2 coord_4) {
        vec2 uv_5 = get_uv_0_vec2_to_vec2_43(coord_4);
        vec2 anf_50 = (2. * u_mouse);
        vec2 anf_51 = (anf_50 - u_resolution);
        float anf_52 = u_resolution[1];
        vec2 mouseUV_6 = (anf_51 / anf_52);
        float anf_53 = (u_time * 2.);
        float anf_54 = sin(anf_53);
        float anf_55 = (anf_54 * 0.1);
        float radius_7 = (anf_55 + 0.15);
        float anf_56 = distance(uv_5, mouseUV_6);
        bool anf_57 = (anf_56 < radius_7);
        if (anf_57) {
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
    float hash_10_435(vec3 p_11) {
        vec3 anf_438 = vec3(127.1, 311.7, 74.7);
        float d_12 = dot(p_11, anf_438);
        float anf_439 = sin(d_12);
        float anf_440 = (anf_439 * 43758.5453);
        return fract(anf_440);
    }
    const vec3 landColor_54 = vec3(0.15, 0.35, 0.1);
    const vec3 mountColor_55 = vec3(0.4, 0.3, 0.2);
    float noise3d_5(vec3 p_6) {
        vec3 i_7 = floor(p_6);
        vec3 f_8 = fract(p_6);
        vec3 anf_441 = (f_8 * f_8);
        vec3 anf_442 = (2. * f_8);
        vec3 anf_443 = (3. - anf_442);
        vec3 u_9 = (anf_441 * anf_443);
        float a_13 = hash_10_435(i_7);
        vec3 anf_444 = vec3(1., 0., 0.);
        vec3 anf_445 = (i_7 + anf_444);
        float b_14 = hash_10_435(anf_445);
        vec3 anf_446 = vec3(0., 1., 0.);
        vec3 anf_447 = (i_7 + anf_446);
        float c_15 = hash_10_435(anf_447);
        vec3 anf_448 = vec3(1., 1., 0.);
        vec3 anf_449 = (i_7 + anf_448);
        float d_16 = hash_10_435(anf_449);
        vec3 anf_450 = vec3(0., 0., 1.);
        vec3 anf_451 = (i_7 + anf_450);
        float e_17 = hash_10_435(anf_451);
        vec3 anf_452 = vec3(1., 0., 1.);
        vec3 anf_453 = (i_7 + anf_452);
        float f_18 = hash_10_435(anf_453);
        vec3 anf_454 = vec3(0., 1., 1.);
        vec3 anf_455 = (i_7 + anf_454);
        float g_19 = hash_10_435(anf_455);
        vec3 anf_456 = vec3(1., 1., 1.);
        vec3 anf_457 = (i_7 + anf_456);
        float h_20 = hash_10_435(anf_457);
        float anf_458 = u_9[0];
        float ab_21 = mix(a_13, b_14, anf_458);
        float anf_459 = u_9[0];
        float cd_22 = mix(c_15, d_16, anf_459);
        float anf_460 = u_9[0];
        float ef_23 = mix(e_17, f_18, anf_460);
        float anf_461 = u_9[0];
        float gh_24 = mix(g_19, h_20, anf_461);
        float anf_462 = u_9[1];
        float abcd_25 = mix(ab_21, cd_22, anf_462);
        float anf_463 = u_9[1];
        float efgh_26 = mix(ef_23, gh_24, anf_463);
        float anf_464 = u_9[2];
        return mix(abcd_25, efgh_26, anf_464);
    }
    float fbm_27(vec3 p_28) {
        vec3 anf_465 = (p_28 * 1.);
        float anf_466 = noise3d_5(anf_465);
        float anf_467 = (anf_466 * 0.5);
        vec3 anf_468 = (p_28 * 2.);
        float anf_469 = noise3d_5(anf_468);
        float anf_470 = (anf_469 * 0.25);
        float anf_471 = (anf_467 + anf_470);
        vec3 anf_472 = (p_28 * 4.);
        float anf_473 = noise3d_5(anf_472);
        float anf_474 = (anf_473 * 0.125);
        float anf_475 = (anf_471 + anf_474);
        vec3 anf_476 = (p_28 * 8.);
        float anf_477 = noise3d_5(anf_476);
        float anf_478 = (anf_477 * 0.0625);
        float anf_479 = (anf_475 + anf_478);
        vec3 anf_480 = (p_28 * 16.);
        float anf_481 = noise3d_5(anf_480);
        float anf_482 = (anf_481 * 0.03125);
        return (anf_479 + anf_482);
    }
    vec2 rotate_0(vec2 p_1, float angle_2) {
        float s_3 = sin(angle_2);
        float c_4 = cos(angle_2);
        float anf_483 = p_1[0];
        float anf_484 = (anf_483 * c_4);
        float anf_485 = p_1[1];
        float anf_486 = (anf_485 * s_3);
        float anf_487 = (anf_484 - anf_486);
        float anf_488 = p_1[0];
        float anf_489 = (anf_488 * s_3);
        float anf_490 = p_1[1];
        float anf_491 = (anf_490 * c_4);
        float anf_492 = (anf_489 + anf_491);
        return vec2(anf_487, anf_492);
    }
    vec3 rotate_by_mouse_61_vec3_to_vec3_434_437(vec2 mouseUV_60, vec3 ray_62) {
        float anf_493 = mouseUV_60[1];
        float anf_494 = (-1. * anf_493);
        float rotX_63 = (anf_494 * 1.5);
        float anf_495 = ray_62[1];
        float anf_496 = ray_62[2];
        vec2 anf_497 = vec2(anf_495, anf_496);
        vec2 ro_yz_64 = rotate_0(anf_497, rotX_63);
        float anf_498 = mouseUV_60[0];
        float anf_499 = (-1. * anf_498);
        float rotY_65 = (anf_499 * 1.5);
        float anf_500 = ray_62[0];
        float anf_501 = ro_yz_64[1];
        vec2 anf_502 = vec2(anf_500, anf_501);
        vec2 ro_xz_66 = rotate_0(anf_502, rotY_65);
        float anf_503 = ro_xz_66[0];
        float anf_504 = ro_yz_64[0];
        float anf_505 = ro_xz_66[1];
        return vec3(anf_503, anf_504, anf_505);
    }
    float sdPlanet_29(vec3 p_30, float radius_31) {
        float len_32 = length(p_30);
        vec3 dir_33 = (p_30 / len_32);
        vec3 anf_506 = (dir_33 * 3.);
        float anf_507 = fbm_27(anf_506);
        float terrain_34 = (anf_507 * 0.4);
        float anf_508 = (len_32 - radius_31);
        return (anf_508 - terrain_34);
    }
    float map_35(vec3 p_36) {
        return sdPlanet_29(p_36, 1.5);
    }
    vec3 getNormal_37(vec3 p_38) {
        float e_39 = 0.002;
        vec3 e_x_40 = vec3(e_39, 0., 0.);
        vec3 e_y_41 = vec3(0., e_39, 0.);
        vec3 e_z_42 = vec3(0., 0., e_39);
        vec3 anf_509 = (p_38 + e_x_40);
        float anf_510 = map_35(anf_509);
        vec3 anf_511 = (p_38 - e_x_40);
        float anf_512 = map_35(anf_511);
        float dx_43 = (anf_510 - anf_512);
        vec3 anf_513 = (p_38 + e_y_41);
        float anf_514 = map_35(anf_513);
        vec3 anf_515 = (p_38 - e_y_41);
        float anf_516 = map_35(anf_515);
        float dy_44 = (anf_514 - anf_516);
        vec3 anf_517 = (p_38 + e_z_42);
        float anf_518 = map_35(anf_517);
        vec3 anf_519 = (p_38 - e_z_42);
        float anf_520 = map_35(anf_519);
        float dz_45 = (anf_518 - anf_520);
        vec3 anf_521 = vec3(dx_43, dy_44, dz_45);
        return normalize(anf_521);
    }
    const vec3 snowColor_56 = vec3(0.85, 0.85, 0.9);
    uniform vec2 u_mouse;
    uniform vec2 u_resolution;
    struct v_option_float {
        int tag;
        float Some_0;
    };
    v_option_float march_49_436(vec3 rd_48, vec3 ro_47, float t_50, int steps_51) {
        int _iter_565 = 0;
        while ((_iter_565 < 1000)) {
            bool anf_522 = (steps_51 > 120);
            if (anf_522) {
                return v_option_float(1, 0.);
            } else {
                vec3 anf_523 = (rd_48 * t_50);
                vec3 anf_524 = (ro_47 + anf_523);
                float d_52 = map_35(anf_524);
                bool anf_525 = (d_52 < 0.0005);
                if (anf_525) {
                    return v_option_float(0, t_50);
                } else {
                    bool anf_526 = (t_50 > 50.);
                    if (anf_526) {
                        return v_option_float(1, 0.);
                    } else {
                        float anf_527 = (d_52 * 0.8);
                        float anf_528 = (t_50 + anf_527);
                        int anf_529 = (steps_51 + 1);
                        rd_48 = rd_48;
                        ro_47 = ro_47;
                        t_50 = anf_528;
                        steps_51 = anf_529;
                        int _iter_inc_566 = (_iter_565 + 1);
                        _iter_565 = _iter_inc_566;
                        continue;
                    }
                }
            }
        }
        v_option_float _tmp_568;
        return _tmp_568;
    }
    v_option_float march_46(vec3 ro_47, vec3 rd_48) {
        return march_49_436(rd_48, ro_47, 0., 0);
    }
    vec3 main_pure(vec2 coord_57) {
        float anf_530 = u_resolution[0];
        float anf_531 = u_resolution[1];
        float res_min_58 = min(anf_530, anf_531);
        vec2 anf_532 = (coord_57 * 2.);
        vec2 anf_533 = (anf_532 - u_resolution);
        vec2 uv_59 = (anf_533 / res_min_58);
        vec2 anf_534 = (u_mouse * 2.);
        vec2 anf_535 = (anf_534 - u_resolution);
        vec2 mouseUV_60 = (anf_535 / res_min_58);
        vec3 anf_536 = vec3(0., 0., -4.);
        vec3 ro_67 = rotate_by_mouse_61_vec3_to_vec3_434_437(mouseUV_60, anf_536);
        float anf_537 = uv_59[0];
        float anf_538 = uv_59[1];
        vec3 anf_539 = vec3(anf_537, anf_538, 1.5);
        vec3 anf_540 = normalize(anf_539);
        vec3 rd_68 = rotate_by_mouse_61_vec3_to_vec3_434_437(mouseUV_60, anf_540);
        v_option_float t_69 = march_46(ro_67, rd_68);
        int _lv_tag_567 = t_69.tag;
        switch (_lv_tag_567) {
            case 1: {
                return vec3(0., 0., 0.);
                break;
            }
            default: {
                float t_70 = t_69.Some_0;
                vec3 anf_541 = (rd_68 * t_70);
                vec3 hitPos_71 = (ro_67 + anf_541);
                vec3 n_72 = getNormal_37(hitPos_71);
                vec3 anf_542 = vec3(1., 0.8, -0.5);
                vec3 lightDir_73 = normalize(anf_542);
                float anf_543 = dot(n_72, lightDir_73);
                float diff_74 = max(anf_543, 0.);
                float ambient_75 = 0.08;
                float anf_544 = length(hitPos_71);
                vec3 dir_76 = (hitPos_71 / anf_544);
                vec3 anf_545 = (dir_76 * 3.);
                float rawHeight_77 = fbm_27(anf_545);
                float seaLevel_78 = 0.35;
                float anf_546 = (rawHeight_77 - seaLevel_78);
                float anf_547 = (1. - seaLevel_78);
                float anf_548 = (anf_546 / anf_547);
                float h_norm_79 = clamp(anf_548, 0., 1.);
                bool anf_549 = (h_norm_79 < 0.3);
                vec3 baseColor_80;
                if (anf_549) {
                    float anf_550 = (h_norm_79 / 0.3);
                    baseColor_80 = mix(deepColor_53, landColor_54, anf_550);
                } else {
                    bool anf_551 = (h_norm_79 < 0.6);
                    if (anf_551) {
                        float anf_552 = (h_norm_79 - 0.3);
                        float anf_553 = (anf_552 / 0.3);
                        baseColor_80 = mix(landColor_54, mountColor_55, anf_553);
                    } else {
                        float anf_554 = (h_norm_79 - 0.6);
                        float anf_555 = (anf_554 / 0.4);
                        baseColor_80 = mix(mountColor_55, snowColor_56, anf_555);
                    }
                }
                vec3 anf_556 = (rd_68 * -1.);
                float anf_557 = dot(n_72, anf_556);
                float anf_558 = max(anf_557, 0.);
                float fresnel_81 = (1. - anf_558);
                float anf_559 = (fresnel_81 * fresnel_81);
                float anf_560 = (anf_559 * fresnel_81);
                float rim_82 = (anf_560 * 0.4);
                vec3 atmoColor_83 = vec3(0.3, 0.5, 1.);
                float anf_561 = (diff_74 * 0.9);
                float anf_562 = (anf_561 + ambient_75);
                vec3 anf_563 = (baseColor_80 * anf_562);
                vec3 anf_564 = (atmoColor_83 * rim_82);
                return (anf_563 + anf_564);
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
        vec3 anf_262 = (cfg_12 + t_11);
        vec3 anf_263 = (anf_262 * 6.28318);
        vec3 anf_264 = cos(anf_263);
        vec3 anf_265 = (anf_264 * 0.5);
        return (anf_265 + 0.5);
    }
    vec2 rotate_0(vec2 p_1, float angle_2) {
        float s_3 = sin(angle_2);
        float c_4 = cos(angle_2);
        float anf_266 = p_1[0];
        float anf_267 = (anf_266 * c_4);
        float anf_268 = p_1[1];
        float anf_269 = (anf_268 * s_3);
        float anf_270 = (anf_267 - anf_269);
        float anf_271 = p_1[0];
        float anf_272 = (anf_271 * s_3);
        float anf_273 = p_1[1];
        float anf_274 = (anf_273 * c_4);
        float anf_275 = (anf_272 + anf_274);
        return vec2(anf_270, anf_275);
    }
    float sMin_5(float a_6, float b_7) {
        float k_8 = 0.1;
        float anf_276 = (b_7 - a_6);
        float anf_277 = (0.5 * anf_276);
        float anf_278 = (anf_277 / k_8);
        float anf_279 = (0.5 + anf_278);
        float h_9 = clamp(anf_279, 0., 1.);
        float anf_280 = mix(b_7, a_6, h_9);
        float anf_281 = (k_8 * h_9);
        float anf_282 = (1. - h_9);
        float anf_283 = (anf_281 * anf_282);
        return (anf_280 - anf_283);
    }
    float sdTorus_13(vec3 p_14, vec2 t_15) {
        float anf_284 = p_14[0];
        float anf_285 = p_14[2];
        vec2 anf_286 = vec2(anf_284, anf_285);
        float anf_287 = length(anf_286);
        float anf_288 = t_15[0];
        float anf_289 = (anf_287 - anf_288);
        float anf_290 = p_14[1];
        vec2 q_16 = vec2(anf_289, anf_290);
        float anf_291 = length(q_16);
        float anf_292 = t_15[1];
        return (anf_291 - anf_292);
    }
    uniform vec2 u_mouse;
    uniform vec2 u_resolution;
    uniform float u_time;
    float map_17(vec3 p_18) {
        float angle_19 = (u_time * 2.);
        float anf_293 = p_18[0];
        float anf_294 = p_18[1];
        vec2 anf_295 = vec2(anf_293, anf_294);
        vec2 p_xy_20 = rotate_0(anf_295, angle_19);
        float anf_296 = p_xy_20[0];
        float anf_297 = p_xy_20[1];
        float anf_298 = p_18[2];
        vec3 p_prime_21 = vec3(anf_296, anf_297, anf_298);
        float anf_299 = p_prime_21[1];
        float anf_300 = p_prime_21[2];
        vec2 anf_301 = vec2(anf_299, anf_300);
        vec2 p_yz_22 = rotate_0(anf_301, angle_19);
        float anf_302 = p_prime_21[0];
        float anf_303 = p_yz_22[0];
        float anf_304 = p_yz_22[1];
        vec3 p_prime_23 = vec3(anf_302, anf_303, anf_304);
        vec2 anf_305 = vec2(1., 0.3);
        float anf_306 = sdTorus_13(p_prime_23, anf_305);
        vec2 anf_307 = vec2(2., 0.5);
        float anf_308 = sdTorus_13(p_18, anf_307);
        return sMin_5(anf_306, anf_308);
    }
    struct v_option_float {
        int tag;
        float Some_0;
    };
    v_option_float march_27_261(vec3 rd_26, vec3 ro_25, float t_28, int steps_29) {
        int _iter_355 = 0;
        while ((_iter_355 < 1000)) {
            bool anf_309 = (steps_29 > 80);
            if (anf_309) {
                return v_option_float(1, 0.);
            } else {
                vec3 anf_310 = (rd_26 * t_28);
                vec3 anf_311 = (ro_25 + anf_310);
                float d_30 = map_17(anf_311);
                bool anf_312 = (d_30 < 0.001);
                if (anf_312) {
                    return v_option_float(0, t_28);
                } else {
                    bool anf_313 = (t_28 > 100.);
                    if (anf_313) {
                        return v_option_float(1, 0.);
                    } else {
                        float anf_314 = (t_28 + d_30);
                        int anf_315 = (steps_29 + 1);
                        rd_26 = rd_26;
                        ro_25 = ro_25;
                        t_28 = anf_314;
                        steps_29 = anf_315;
                        int _iter_inc_356 = (_iter_355 + 1);
                        _iter_355 = _iter_inc_356;
                        continue;
                    }
                }
            }
        }
        v_option_float _tmp_358;
        return _tmp_358;
    }
    v_option_float march_24(vec3 ro_25, vec3 rd_26) {
        return march_27_261(rd_26, ro_25, 0., 0);
    }
    vec3 main_pure(vec2 coord_31) {
        float anf_316 = u_resolution[0];
        float anf_317 = u_resolution[1];
        float res_min_32 = min(anf_316, anf_317);
        vec2 anf_318 = (coord_31 * 2.);
        vec2 anf_319 = (anf_318 - u_resolution);
        vec2 uv_33 = (anf_319 / res_min_32);
        vec2 anf_320 = (u_mouse * 2.);
        vec2 anf_321 = (anf_320 - u_resolution);
        vec2 mouseUV_34 = (anf_321 / res_min_32);
        vec3 ro_init_35 = vec3(0., 0., -6.);
        float anf_322 = uv_33[0];
        float anf_323 = uv_33[1];
        vec3 anf_324 = vec3(anf_322, anf_323, 1.);
        vec3 rd_init_36 = normalize(anf_324);
        float anf_325 = mouseUV_34[1];
        float rotX_37 = (-1. * anf_325);
        float anf_326 = mouseUV_34[0];
        float rotY_38 = (-1. * anf_326);
        float anf_327 = ro_init_35[1];
        float anf_328 = ro_init_35[2];
        vec2 anf_329 = vec2(anf_327, anf_328);
        vec2 ro_yz_39 = rotate_0(anf_329, rotX_37);
        float anf_330 = rd_init_36[1];
        float anf_331 = rd_init_36[2];
        vec2 anf_332 = vec2(anf_330, anf_331);
        vec2 rd_yz_40 = rotate_0(anf_332, rotX_37);
        float anf_333 = ro_init_35[0];
        float anf_334 = ro_yz_39[0];
        float anf_335 = ro_yz_39[1];
        vec3 ro_41 = vec3(anf_333, anf_334, anf_335);
        float anf_336 = rd_init_36[0];
        float anf_337 = rd_yz_40[0];
        float anf_338 = rd_yz_40[1];
        vec3 rd_42 = vec3(anf_336, anf_337, anf_338);
        float anf_339 = ro_41[0];
        float anf_340 = ro_41[2];
        vec2 anf_341 = vec2(anf_339, anf_340);
        vec2 ro_xz_43 = rotate_0(anf_341, rotY_38);
        float anf_342 = rd_42[0];
        float anf_343 = rd_42[2];
        vec2 anf_344 = vec2(anf_342, anf_343);
        vec2 rd_xz_44 = rotate_0(anf_344, rotY_38);
        float anf_345 = ro_xz_43[0];
        float anf_346 = ro_41[1];
        float anf_347 = ro_xz_43[1];
        vec3 ro_45 = vec3(anf_345, anf_346, anf_347);
        float anf_348 = rd_xz_44[0];
        float anf_349 = rd_42[1];
        float anf_350 = rd_xz_44[1];
        vec3 rd_46 = vec3(anf_348, anf_349, anf_350);
        v_option_float anf_351 = march_24(ro_45, rd_46);
        int _lv_tag_357 = anf_351.tag;
        vec3 col_47;
        switch (_lv_tag_357) {
            case 1: {
                col_47 = vec3(0.2, 0.2, 0.2);
                break;
            }
            default: {
                float t_48 = anf_351.Some_0;
                float anf_352 = (t_48 * 0.3);
                col_47 = palette_10(anf_352);
                break;
            }
        }
        vec2 anf_353 = (uv_33 - mouseUV_34);
        float anf_354 = length(anf_353);
        float glow_49 = (0.02 / anf_354);
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
    float gcd_8_float_to_float_to_float_90(float a_9, float b_10) {
        int _iter_111 = 0;
        while ((_iter_111 < 1000)) {
            bool anf_91 = (a_9 < 0.05);
            if (anf_91) {
                return b_10;
            } else {
                bool anf_92 = (b_10 < 0.05);
                if (anf_92) {
                    return a_9;
                } else {
                    bool anf_93 = (a_9 > b_10);
                    if (anf_93) {
                        float anf_94 = (a_9 - b_10);
                        a_9 = anf_94;
                        b_10 = b_10;
                        int _iter_inc_112 = (_iter_111 + 1);
                        _iter_111 = _iter_inc_112;
                        continue;
                    } else {
                        float anf_95 = (b_10 - a_9);
                        a_9 = a_9;
                        b_10 = anf_95;
                        int _iter_inc_113 = (_iter_111 + 1);
                        _iter_111 = _iter_inc_113;
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
        float anf_96 = (-1. * s_6);
        vec2 anf_97 = vec2(c_7, anf_96);
        vec2 anf_98 = vec2(s_6, c_7);
        return mat2(anf_97, anf_98);
    }
    uniform vec2 u_resolution;
    vec2 get_uv_0(vec2 coord_1) {
        vec2 anf_99 = (2. * coord_1);
        vec2 top_2 = (anf_99 - u_resolution);
        float anf_100 = u_resolution[0];
        float anf_101 = u_resolution[1];
        float bot_3 = min(anf_100, anf_101);
        return (top_2 / bot_3);
    }
    uniform float u_time;
    vec3 main_pure(vec2 coord_11) {
        mat2 anf_102 = rotate_4(u_time);
        vec2 anf_103 = get_uv_0(coord_11);
        vec2 uv_12 = (anf_102 * anf_103);
        float anf_104 = (u_time * 2.);
        float anf_105 = sin(anf_104);
        vec2 anf_106 = (uv_12 * anf_105);
        vec2 anf_107 = (anf_106 * 2.);
        vec2 anf_108 = abs(anf_107);
        float x_13 = anf_108[0];
        float y_14 = anf_108[1];
        float res_15 = gcd_8_float_to_float_to_float_90(x_13, y_14);
        float anf_109 = (res_15 * 0.5);
        float anf_110 = (1. - res_15);
        return vec3(res_15, anf_109, anf_110);
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
