set(CROSS_CHECK_TESTS_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(CROSS_CHECK_TESTS_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})

list(APPEND CROSS_CHECK_TEST_DEPS
    CrossChecks
    runtime
    )

configure_lit_site_cfg(
  ${CMAKE_CURRENT_SOURCE_DIR}/lit.site.cfg.py.in
  ${CMAKE_CURRENT_BINARY_DIR}/lit.site.cfg.py
  MAIN_CONFIG
  ${CMAKE_CURRENT_SOURCE_DIR}/lit.cfg.py
  )

add_lit_testsuite(check-cross-checks "Running cross-checks test suite"
  ${CMAKE_CURRENT_BINARY_DIR}
  DEPENDS ${CROSS_CHECK_TEST_DEPS}
  )
set_target_properties(check-cross-checks PROPERTIES FOLDER "Cross-check plugin tests")
