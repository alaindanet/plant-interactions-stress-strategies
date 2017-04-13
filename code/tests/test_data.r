
# Test the filtering, gathering, joining process in march file
expect_equal(nrow(march_sp), nrow(march_h))
expect_equal(nrow(march_d), nrow(march_h))
expect_equal(nrow(march_d), nrow(march_hm))
expect_equal(nrow(march_sp), nrow(march_hm))
expect_equal(nrow(march), nrow(march_sp))

