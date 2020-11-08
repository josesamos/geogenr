context("test uscb_acs_5ye")

test_that("uscb_acs_5ye works", {
  folder <- "../geodimension/data/us/"
  ua <- uscb_acs_5ye(folder = folder)

  expect_equal(ua, structure(
    list(
      folder = "../geodimension/data/us/",
      years = 2010:2018,
      url = "https://www2.census.gov/geo/tiger/TIGER_DP/%dACS/",
      extension = ".gdb.zip",
      variables = structure(
        list(
          type = c(1,
                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2,
                   2, 2),
          name = c(
            "American Indian/Alaska Native/Native Hawaiian Area",
            "Alaska Native Regional Corporation",
            "Congressional District (116th Congress)",
            "County",
            "Place",
            "Elementary School District",
            "Secondary School District",
            "Unified School District",
            "State",
            "State Legislative Districts - Upper Chamber",
            "State Legislative Districts - Lower Chamber",
            "Zip Code Tabulation Area",
            "Tribal Block Group",
            "Tribal Census Tract",
            "New England City and Town Area",
            "New England City and Town Area Division",
            "Combined New England City and Town Area",
            "Metropolitan/Micropolitan Statistical Area",
            "Metropolitan Division",
            "Combined Statistical Area",
            "Public Use Microdata Area",
            "Urban Area"
          ),
          url = c(
            "ACS_%d_5YR_AIARES",
            "ACS_%d_5YR_ANRC",
            "ACS_%d_5YR_CD_116",
            "ACS_%d_5YR_COUNTY",
            "ACS_%d_5YR_PLACE",
            "ACS_%d_5YR_SDE",
            "ACS_%d_5YR_SDS",
            "ACS_%d_5YR_SDU",
            "ACS_%d_5YR_STATE",
            "ACS_%d_5YR_SLDU",
            "ACS_%d_5YR_SLDL",
            "ACS_%d_5YR_ZCTA",
            "ACS_%d_5YR_TBG",
            "ACS_%d_5YR_TTRACT",
            "ACS_%d_5YR_NECTA",
            "ACS_%d_5YR_NECTADIV",
            "ACS_%d_5YR_CNECTA",
            "ACS_%d_5YR_MSA",
            "ACS_%d_5YR_METDIV",
            "ACS_%d_5YR_CSA",
            "ACS_%d_5YR_PUMA",
            "ACS_%d_5YR_UA"
          )
        ),
        row.names = c(NA,-22L),
        class = "data.frame"
      )
    ),
    class = "uscb_acs_5ye"
  ))
})
