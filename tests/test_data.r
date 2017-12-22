
# Test the filtering, gathering, joining process in files
expect_equal(nrow(march_sp), nrow(march_h))
expect_equal(nrow(march_d), nrow(march_h))
expect_equal(nrow(march_d), nrow(march_hm))
expect_equal(nrow(march_sp), nrow(march_hm))

expect_equal(nrow(january_sp), nrow(january_h))
expect_equal(nrow(january_d), nrow(january_h))
expect_equal(nrow(january_d), nrow(january_hm))
expect_equal(nrow(january_sp), nrow(january_hm))

expect_equal(nrow(june_sp), nrow(june_h))
expect_equal(nrow(june_d), nrow(june_h))
expect_equal(nrow(june_d), nrow(june_hm))
expect_equal(nrow(june_sp), nrow(june_hm))

expect_equal(nrow(november_sp), nrow(november_h))
expect_equal(nrow(november_d), nrow(november_h))
expect_equal(nrow(november_d), nrow(november_hm))
expect_equal(nrow(november_sp), nrow(november_hm))

# Test the regrouping process
sum_row <- nrow(january_surv2) + nrow(mar_surv2) + nrow(june_surv2) + nrow(november_surv2)
expect_equal(sum_row, nrow(try1))

