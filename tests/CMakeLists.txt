# This is the CMake build file for the test directory of NCEPLIBS-w3emc.

# Kyle Gerheiser, Ed Hartnett, Alex Richert

execute_process(COMMAND cmake -E create_symlink
  "${CMAKE_CURRENT_SOURCE_DIR}/data"
  "${CMAKE_CURRENT_BINARY_DIR}/data"
)

# Custom target to generate procedure lists for each kind
foreach(kind ${kinds})
  set(PROCEDURE_LIST_FILE "${CMAKE_CURRENT_BINARY_DIR}/procedure_list_${kind}.txt")
  
  add_custom_command(
    OUTPUT ${PROCEDURE_LIST_FILE}
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/list_procedures.sh 
            ${CMAKE_SOURCE_DIR}/src 
            ${PROCEDURE_LIST_FILE}
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/list_procedures.sh
    COMMENT "Generating procedure list for kind ${kind}"
    VERBATIM
  )
  
  add_custom_target(generate_procedure_list_${kind}
    DEPENDS ${PROCEDURE_LIST_FILE}
  )
endforeach()

function(create_test name kind use_module)
  # Optional 4th argument: use_shell_wrapper (defaults to FALSE)
  set(use_shell_wrapper FALSE)
  if(ARGC GREATER 3)
    set(use_shell_wrapper ${ARGV3})
  endif()
  
  if(use_module)
    set(test_suffix "_${kind}_module")
    set(exe_name "${name}${test_suffix}")
    set(test_label "module")
  else()
    set(test_suffix "_${kind}")
    set(exe_name "${name}${test_suffix}")
    set(test_label "nomodule")
  endif()
  
  add_executable(${exe_name} ${name}.F90)
  set_target_properties(${exe_name} PROPERTIES COMPILE_FLAGS "${fortran_${kind}_flags}")
  # Convert kind to uppercase for preprocessor definitions
  string(TOUPPER ${kind} kind_upper)
  target_compile_definitions(${exe_name} PUBLIC -DKIND_${kind_upper})
  
  if(use_module)
    # Module-based tests
    set_target_properties(${exe_name} PROPERTIES Fortran_MODULE_DIRECTORY
      ${CMAKE_CURRENT_BINARY_DIR}/include_${kind})
    target_compile_definitions(${exe_name} PUBLIC -DUSE_W3EMC_MODULE)
    target_include_directories(${exe_name} PRIVATE 
      ${CMAKE_BINARY_DIR}/src/include_${kind})
    # Add explicit dependencies on module object and library to ensure they're built first
    add_dependencies(${exe_name} w3emc_${kind}_module_obj w3emc_${kind})
    # Link library for all tests (shell wrapper only affects test execution, not linking)
    target_link_libraries(${exe_name} PRIVATE w3emc_${kind})
  else()
    # Traditional tests - always link against the library
    target_link_libraries(${exe_name} PRIVATE w3emc_${kind})
  endif()
  
  # Add test with appropriate command
  if(use_shell_wrapper)
    add_test(NAME ${exe_name} COMMAND ${CMAKE_SOURCE_DIR}/tests/${name}.sh ${kind})
  else()
    add_test(NAME ${exe_name} COMMAND ${exe_name})
  endif()
  
  set_tests_properties(${exe_name} PROPERTIES LABELS "${test_label};kind_${kind}")
endfunction()

# These are the tests.
foreach(kind ${kinds})
  # Run all tests twice: once traditional, once with module
  foreach(use_module FALSE TRUE)
    create_test(test_summary ${kind} ${use_module})
    create_test(test_mkfldsep ${kind} ${use_module})
    create_test(test_gbytec ${kind} ${use_module})
    create_test(test_w3tagb ${kind} ${use_module})
    create_test(test_w3fi71 ${kind} ${use_module})
#    create_test(test_w3fi72 ${kind} ${use_module})
    create_test(test_w3fi73 ${kind} ${use_module})
    create_test(test_w3fi74 ${kind} ${use_module})
    create_test(test_fparsei ${kind} ${use_module})
    create_test(test_fparser ${kind} ${use_module})
    create_test(test_gbyte ${kind} ${use_module})
    create_test(test_getgbemh ${kind} ${use_module})
    create_test(test_errexit ${kind} ${use_module} TRUE)
    create_test(test_errmsg ${kind} ${use_module} TRUE)
    create_test(test_w3fi63 ${kind} ${use_module})
    create_test(test_fi637 ${kind} ${use_module})
    create_test(test_xmovex ${kind} ${use_module})
    create_test(test_xstore ${kind} ${use_module})
    create_test(test_w3movdat ${kind} ${use_module})
    create_test(test_w3kind ${kind} ${use_module})
  endforeach()
  
  # The module test only runs with module enabled
  create_test(test_w3emc_module ${kind} TRUE)
  
  # Procedure availability test - compile-time only
  # The executable is compiled (which validates procedure availability through the "only" imports)
  # but not run (as runtime execution adds no value to the test)
  set(exe_name "test_procedure_availability_${kind}_module")
  set(PROCEDURE_LIST_FILE "${CMAKE_CURRENT_BINARY_DIR}/procedure_list_${kind}.txt")
  set(TEST_SOURCE_FILE "${CMAKE_CURRENT_BINARY_DIR}/test_procedure_availability_${kind}.F90")
  set(TEST_TEMPLATE_FILE "${CMAKE_CURRENT_SOURCE_DIR}/test_procedure_availability.F90.in")
  
  # Create a custom command to generate the test source file from template
  # This runs at build time and reads the procedure list that was just generated
  add_custom_command(
    OUTPUT ${TEST_SOURCE_FILE}
    COMMAND ${CMAKE_COMMAND}
      -DTEMPLATE_FILE=${TEST_TEMPLATE_FILE}
      -DOUTPUT_FILE=${TEST_SOURCE_FILE}
      -DPROCEDURE_LIST_FILE=${PROCEDURE_LIST_FILE}
      -P ${CMAKE_CURRENT_SOURCE_DIR}/generate_test_from_template.cmake
    DEPENDS ${PROCEDURE_LIST_FILE} ${TEST_TEMPLATE_FILE} ${CMAKE_CURRENT_SOURCE_DIR}/generate_test_from_template.cmake
    COMMENT "Generating procedure availability test for kind ${kind}"
    VERBATIM
  )
  
  add_executable(${exe_name} ${TEST_SOURCE_FILE})
  target_link_libraries(${exe_name} PRIVATE w3emc_${kind})
  set_target_properties(${exe_name} PROPERTIES COMPILE_FLAGS "${fortran_${kind}_flags}")
  set_target_properties(${exe_name} PROPERTIES Fortran_MODULE_DIRECTORY
    ${CMAKE_CURRENT_BINARY_DIR}/include_${kind})
  # Convert kind to uppercase for preprocessor definitions
  string(TOUPPER ${kind} kind_upper)
  target_compile_definitions(${exe_name} PUBLIC -DKIND_${kind_upper})
  target_compile_definitions(${exe_name} PUBLIC -DUSE_W3EMC_MODULE)
  target_include_directories(${exe_name} PRIVATE 
    ${CMAKE_BINARY_DIR}/src/include_${kind})
  
  # Make the executable depend on the procedure list generation, module object, and library
  add_dependencies(${exe_name} generate_procedure_list_${kind})
  add_dependencies(${exe_name} w3emc_${kind}_module_obj w3emc_${kind})
  
  # Note: The test is the compilation of ${exe_name}, not running it.
  # If any procedure in the "only" clause is missing from the module,
  # compilation will fail. This validates all procedures are available.
  
  # Note: The test is the compilation of ${exe_name}, not running it.
  # If any procedure in the "only" clause is missing from the module,
  # compilation will fail. This validates all procedures are available.
endforeach()  
