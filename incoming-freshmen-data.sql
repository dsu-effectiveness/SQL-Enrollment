  WITH enrolled_students AS (
      SELECT DISTINCT
             a.sfrstcr_term_code,
             a.sfrstcr_pidm
        FROM sfrstcr a
       WHERE a.sfrstcr_levl_code = 'UG'
         AND a.sfrstcr_rsts_code IN (SELECT b.stvrsts_code
                                       FROM stvrsts b
                                      WHERE b.stvrsts_incl_sect_enrl = 'Y')
      ),
student_list AS (
      SELECT f.sfrstcr_term_code AS term_code,
             g.stvterm_desc,
             f.sgbstdn_levl_code AS levl_code,
             h.stvlevl_desc,
             f.sgbstdn_coll_code_1 AS coll_code,
             j.stvcoll_desc,
             f.sgbstdn_pidm AS student_pidm,
             f.sgbstdn_styp_code,
             x.spriden_id,
             f.sgbstdn_program,
             CASE WHEN y.sgbstdn_pidm IS NOT NULL THEN 'Y'
                  ELSE 'N'
                  END AS fterm_ind
        FROM (SELECT b.sfrstcr_term_code,
                     a.sgbstdn_pidm,
                     a.sgbstdn_levl_code,
                     a.sgbstdn_styp_code,
                     CASE a.sgbstdn_coll_code_1
                          WHEN 'NS' THEN 'SC' -- Natural Sci into Sci, Engr, & Tech
                          WHEN 'CT' THEN 'SC' -- Computer Info Tech into Sci, Engr, & Tech
                          WHEN 'EF' THEN 'ED' -- Ed/Fam Sci/PE into College of Ed
                          WHEN 'HI' THEN 'HS' -- Hist/Poli Sci into College of Humanities
                          WHEN 'MA' THEN 'SC' -- Math into Sci, Engr, & Tech
                          WHEN 'TE' THEN 'SC' -- Technologies into Sci, Engr, & Tech
                          ELSE a.sgbstdn_coll_code_1
                          END AS sgbstdn_coll_code_1,
                     a.sgbstdn_program_1 AS sgbstdn_program
                FROM sgbstdn a
          INNER JOIN enrolled_students b
                  ON a.sgbstdn_pidm = b.sfrstcr_pidm
               WHERE a.sgbstdn_stst_code = 'AS'
                 AND SUBSTR(b.sfrstcr_term_code, 1, 4) BETWEEN '2012' AND '2021'
                 AND a.sgbstdn_term_code_eff = (SELECT MAX(aa.sgbstdn_term_code_eff)
                                                 FROM sgbstdn aa
                                                WHERE a.sgbstdn_pidm = aa.sgbstdn_pidm
                                                  AND aa.sgbstdn_term_code_eff <= b.sfrstcr_term_code)) f
    LEFT JOIN stvterm g
           ON f.sfrstcr_term_code = g.stvterm_code
    LEFT JOIN stvlevl h
           ON f.sgbstdn_levl_code = h.stvlevl_code
    LEFT JOIN stvcoll j
           ON f.sgbstdn_coll_code_1 = j.stvcoll_code
    LEFT JOIN spriden x
           ON f.sgbstdn_pidm = x.spriden_pidm
          AND x.spriden_change_ind IS NULL
    LEFT JOIN sgbstdn y
           ON f.sgbstdn_pidm = y.sgbstdn_pidm
          AND y.sgbstdn_styp_code = f.sgbstdn_styp_code
          AND y.sgbstdn_term_code_eff = f.sfrstcr_term_code
            ),
stus AS (
    SELECT a.student_pidm,
           a.stvterm_desc,
           a.spriden_id AS dixie_id,
           b.spriden_first_name AS first_name,
           b.spriden_last_name AS last_name,
           f.hsgpact_hsgpact AS index_score,
           a.stvcoll_desc,
           j.smrprle_program_desc AS program,
           h.sfbetrm_initial_reg_date AS initial_reg_date,
           SUM(c.sfrstcr_credit_hr) AS term_registered_credits,
           m.hrs_earned,
           k.registration_holds,
           k.graduation_holds,
           k.hold_description,
           n.waitlisted,
           LISTAGG(x.shrtmcm_comment, ', ') AS ap_courses,
           LISTAGG(CASE WHEN d.ssbsect_subj_code = 'ENGL' THEN d.ssbsect_subj_code || d.ssbsect_crse_numb ELSE NULL
                        END, ', ') WITHIN GROUP (ORDER BY d.ssbsect_crse_numb) AS engl_registered,
           LISTAGG(CASE WHEN d.ssbsect_subj_code = 'MATH' THEN d.ssbsect_subj_code || d.ssbsect_crse_numb ELSE NULL
                        END, ', ') WITHIN GROUP (ORDER BY d.ssbsect_crse_numb) AS math_registered,
           LISTAGG(CASE WHEN (d.ssbsect_subj_code = 'SSC'
                              OR (d.ssbsect_subj_code = 'BUS' AND d.ssbsect_crse_numb = '1050')
                              OR (d.ssbsect_subj_code IN ('MUSC', 'ART', 'THTR', 'DNC') AND d.ssbsect_crse_numb = '1001'))
                        THEN d.ssbsect_subj_code || d.ssbsect_crse_numb ELSE NULL
                        END, ', ') WITHIN GROUP (ORDER BY d.ssbsect_crse_numb) AS fye_registered,
           LISTAGG(CASE WHEN d.ssbsect_subj_code NOT IN ('ENGL', 'MATH', 'SSC')
                             AND d.ssbsect_subj_code || d.ssbsect_crse_numb NOT IN ('BUS1050', 'MUSC1001', 'ART1001', 'THTR1001', 'DNC1001')
                        THEN d.ssbsect_subj_code || d.ssbsect_crse_numb ELSE NULL
                        END, ', ') WITHIN GROUP (ORDER BY d.ssbsect_subj_code, d.ssbsect_crse_numb) AS oth_registered,
           gg.engl_prior,
           gg.math_prior,
           gg.oth_prior
      FROM student_list a
 LEFT JOIN spriden b
        ON a.student_pidm = b.spriden_pidm
       AND b.spriden_change_ind IS NULL
 LEFT JOIN (SELECT *
              FROM sfrstcr
             WHERE sfrstcr_camp_code <> 'XXX'
               AND sfrstcr_levl_code = 'UG'
               AND sfrstcr_term_code = '202140'
               AND sfrstcr_rsts_code IN (SELECT stvrsts_code
                                           FROM stvrsts
                                          WHERE stvrsts_incl_sect_enrl = 'Y')) c
        ON a.term_code = c.sfrstcr_term_code
       AND a.student_pidm = c.sfrstcr_pidm
 LEFT JOIN ssbsect d
        ON c.sfrstcr_crn = d.ssbsect_crn
       AND c.sfrstcr_term_code = d.ssbsect_term_code
       AND d.ssbsect_ssts_code = 'A'
 LEFT JOIN (SELECT cc.sfrstcr_pidm,
                   LISTAGG(CASE WHEN dd.ssbsect_subj_code = 'ENGL' THEN dd.ssbsect_subj_code || dd.ssbsect_crse_numb ELSE NULL
                                END, ', ') WITHIN GROUP (ORDER BY dd.ssbsect_crse_numb) AS engl_prior,
                   LISTAGG(CASE WHEN dd.ssbsect_subj_code = 'MATH' THEN dd.ssbsect_subj_code || dd.ssbsect_crse_numb ELSE NULL
                                END, ', ') WITHIN GROUP (ORDER BY dd.ssbsect_crse_numb) AS math_prior,
                   LISTAGG(CASE WHEN dd.ssbsect_subj_code NOT IN ('ENGL', 'MATH') THEN dd.ssbsect_subj_code || dd.ssbsect_crse_numb ELSE NULL
                                END, ', ' ON overflow truncate without count)
                                WITHIN GROUP (ORDER BY dd.ssbsect_crse_numb) AS oth_prior
              FROM (SELECT DISTINCT
                           sfrstcr_term_code,
                           sfrstcr_pidm,
                           sfrstcr_crn
                      FROM sfrstcr
                     WHERE sfrstcr_camp_code <> 'XXX'
                       AND sfrstcr_levl_code = 'UG'
                       AND sfrstcr_term_code < '202140'
                       AND sfrstcr_rsts_code IN (SELECT stvrsts_code
                                                   FROM stvrsts
                                                  WHERE stvrsts_incl_sect_enrl = 'Y')) cc
                LEFT JOIN ssbsect dd
                       ON cc.sfrstcr_crn = dd.ssbsect_crn
                      AND cc.sfrstcr_term_code = dd.ssbsect_term_code
                      AND dd.ssbsect_ssts_code = 'A'
                    GROUP BY cc.sfrstcr_pidm) gg
        ON a.student_pidm = gg.sfrstcr_pidm
 LEFT JOIN (SELECT mm.sfrstcr_pidm,
                   LISTAGG(CASE WHEN dd.ssbsect_subj_code NOT IN ('ENGL', 'MATH') THEN dd.ssbsect_subj_code || dd.ssbsect_crse_numb ELSE NULL
                                END, ', ' ON overflow truncate without count)
                                WITHIN GROUP (ORDER BY dd.ssbsect_crse_numb) AS waitlisted
              FROM (SELECT DISTINCT
                           sfrstcr_term_code,
                           sfrstcr_pidm,
                           sfrstcr_crn
                      FROM sfrstcr
                     WHERE sfrstcr_camp_code <> 'XXX'
                       AND sfrstcr_levl_code = 'UG'
                       AND sfrstcr_term_code = '202140'
                       AND sfrstcr_rsts_code = 'WL') mm
                LEFT JOIN ssbsect dd
                       ON mm.sfrstcr_crn = dd.ssbsect_crn
                      AND mm.sfrstcr_term_code = dd.ssbsect_term_code
                      AND dd.ssbsect_ssts_code = 'A'
                    GROUP BY mm.sfrstcr_pidm) n
        ON a.student_pidm = n.sfrstcr_pidm
 LEFT JOIN dsc.hsgpact f
        ON a.student_pidm = f.hsgpact_pidm
 LEFT JOIN sgbstdn g
        ON a.student_pidm = g.sgbstdn_pidm
       AND a.term_code = g.sgbstdn_term_code_eff
 LEFT JOIN sfbetrm h
        ON a.student_pidm = h.sfbetrm_pidm
       AND a.term_code = h.sfbetrm_term_code
 LEFT JOIN smrprle j
        ON a.sgbstdn_program = j.smrprle_program
       AND j.smrprle_curr_ind = 'Y'
 LEFT JOIN (SELECT k1.sprhold_pidm,
                   LISTAGG(DISTINCT k2.stvhldd_desc, ', ') AS hold_description,
                   CASE WHEN SUM(CASE WHEN k2.stvhldd_reg_hold_ind = 'Y' THEN 1 ELSE 0 END) > 0
                        THEN 'Y' ELSE 'N'
                        END AS registration_holds,
                   CASE WHEN SUM(CASE WHEN k2.stvhldd_grad_hold_ind = 'Y' THEN 1 ELSE 0 END) > 0
                        THEN 'Y' ELSE 'N'
                        END AS graduation_holds
              FROM sprhold k1
        INNER JOIN stvhldd k2
                ON k1.sprhold_hldd_code = k2.stvhldd_code
             WHERE (k2.stvhldd_reg_hold_ind = 'Y' OR k2.stvhldd_grad_hold_ind = 'Y')
               AND k1.sprhold_release_ind = 'N'
               AND k1.sprhold_from_date <= SYSDATE
               AND k1.sprhold_to_date >= SYSDATE
          GROUP BY k1.sprhold_pidm) k
       ON a.student_pidm = k.sprhold_pidm
LEFT JOIN (SELECT shrlgpa_pidm,
                  shrlgpa_gpa_type_ind,
                  SUM(shrlgpa_hours_earned) AS hrs_earned
             FROM shrlgpa
         GROUP BY shrlgpa_pidm,
                  shrlgpa_gpa_type_ind) m
        ON a.student_pidm = m.shrlgpa_pidm
       AND m.shrlgpa_gpa_type_ind = 'I'
 LEFT JOIN shrtmcm x
        ON a.student_pidm = x.shrtmcm_pidm
       AND x.shrtmcm_comment LIKE '%AP%'
     WHERE a.term_code = '202140'
       AND a.sgbstdn_styp_code = 'F'
       AND a.fterm_ind = 'Y'
  GROUP BY a.student_pidm,
           a.stvterm_desc,
           a.spriden_id,
           b.spriden_first_name,
           b.spriden_last_name,
           f.hsgpact_hsgpact,
           a.stvcoll_desc,
           j.smrprle_program_desc,
           h.sfbetrm_initial_reg_date,
           m.hrs_earned,
           k.registration_holds,
           k.graduation_holds,
           k.hold_description,
           n.waitlisted,
           gg.engl_prior,
           gg.math_prior,
           gg.oth_prior
           )

    SELECT a.stvterm_desc,
           a.dixie_id,
           a.first_name,
           a.last_name,
           CASE WHEN d.spbpers_citz_code = '2' THEN 'Nonresident alien'
                WHEN d.spbpers_ethn_cde = '2' THEN 'Hispanic'
                WHEN j.race_cdes LIKE '%H%' THEN 'Hispanic'
                WHEN j.race_cdes LIKE '%|%' THEN 'Two or more races'
                WHEN j.race_cdes IS NOT NULL THEN k.gorrace_desc
                ELSE 'Unknown'
                END AS race_desc,
           baninst1.gp_goksdif.f_get_sd_text('SPBPERS', 'FIRST_GEN_STUDENT', a.student_pidm, 1) AS first_gen,
           CASE WHEN m.rpratrm_pidm IS NOT NULL THEN 'Y'
                ELSE 'N'
                END AS pell_award,
           p.stvresd_desc,
           a.index_score,
           t.sortest_test_score AS act_score,
           q.sortest_test_score AS act_math,
           s.sortest_test_score AS act_english,
           u.sorhsch_gpa AS hs_gpa,
           v.sortest_test_score AS accuplacer_wri,
           w.sortest_test_score AS aleks,
           a.ap_courses,
           a.stvcoll_desc,
           a.program,
           TRUNC(a.initial_reg_date) AS initial_reg_date,
           a.term_registered_credits,
           CASE WHEN a.term_registered_credits >= 12 THEN 'FT' ELSE 'PT' END AS ft_pt_ind,
           a.hrs_earned,
           r.sfrblpa_blck_code AS block_code,
           a.registration_holds,
           a.graduation_holds,
           a.hold_description,
           a.waitlisted,
           a.engl_registered,
           a.math_registered,
           a.fye_registered,
           a.oth_registered,
           a.engl_prior,
           a.math_prior,
           a.oth_prior
      FROM stus a
 LEFT JOIN spbpers d
        ON a.student_pidm = d.spbpers_pidm
 LEFT JOIN (SELECT jj.gorprac_pidm,
                   LISTAGG(jj.gorprac_race_cde, '|') AS race_cdes
              FROM gorprac jj
          GROUP BY jj.gorprac_pidm) j
        ON a.student_pidm = j.gorprac_pidm
 LEFT JOIN gorrace k
        ON j.race_cdes = k.gorrace_race_cde
 LEFT JOIN rpratrm m
        ON a.student_pidm = m.rpratrm_pidm
       AND m.rpratrm_period = '202140'
       AND m.rpratrm_fund_code = 'FPELL'
       AND m.rpratrm_offer_amt > 0
 LEFT JOIN sgbstdn n
        ON a.student_pidm = n.sgbstdn_pidm
       AND n.sgbstdn_term_code_eff = '202140'
 LEFT JOIN stvresd p
        ON n.sgbstdn_resd_code = p.stvresd_code
 LEFT JOIN (SELECT q2.sortest_pidm,
                   MAX(q2.sortest_test_score) AS sortest_test_score
              FROM sortest q2
             WHERE q2.sortest_tesc_code IN ('A02N','A02')
          GROUP BY q2.sortest_pidm) q
        ON a.student_pidm = q.sortest_pidm
 LEFT JOIN (SELECT s2.sortest_pidm,
                   MAX(s2.sortest_test_score) AS sortest_test_score
              FROM sortest s2
             WHERE s2.sortest_tesc_code IN ('A01')
            GROUP BY s2.sortest_pidm) s
        ON a.student_pidm = s.sortest_pidm
 LEFT JOIN (SELECT t2.sortest_pidm,
                   MAX(t2.sortest_test_score) AS sortest_test_score
              FROM sortest t2
             WHERE t2.sortest_tesc_code IN ('A05')
            GROUP BY t2.sortest_pidm) t
        ON a.student_pidm = t.sortest_pidm
 LEFT JOIN (SELECT v2.sortest_pidm,
                   MAX(v2.sortest_test_score) AS sortest_test_score
              FROM sortest v2
             WHERE v2.sortest_tesc_code IN ('CPTW')
            GROUP BY v2.sortest_pidm) v
        ON a.student_pidm = v.sortest_pidm
 LEFT JOIN (SELECT w2.sortest_pidm,
                   MAX(w2.sortest_test_score) AS sortest_test_score
              FROM sortest w2
             WHERE w2.sortest_tesc_code IN ('ALEKS','ALEKSN')
            GROUP BY w2.sortest_pidm) w
        ON a.student_pidm = w.sortest_pidm
 LEFT JOIN sfrblpa r
        ON a.student_pidm = r.sfrblpa_pidm
       AND r.sfrblpa_term_code = '202140'
 LEFT JOIN (SELECT uu.sorhsch_pidm,
                   CASE WHEN uu.sorhsch_sbgi_code IN ('459995','960000') THEN 'GED'
                        ELSE uu.sorhsch_gpa
                        END AS sorhsch_gpa,
                   ROW_NUMBER() over (PARTITION BY uu.sorhsch_pidm ORDER BY uu.sorhsch_graduation_date DESC) AS rn
              FROM sorhsch uu
             WHERE uu.sorhsch_gpa IS NOT NULL
                OR uu.sorhsch_sbgi_code IN ('459995','960000') ) u
        ON a.student_pidm = u.sorhsch_pidm
       AND u.rn = 1
  ; 
