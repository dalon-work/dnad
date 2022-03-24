project('src', 'fortran',
  version : '0.1')

dnad_nderiv = get_option('DNAD_NDERIV')

if dnad_nderiv == 0 
  error('DNAD_NDERIV must be set!')
endif

dnad_conf_data = configuration_data()
dnad_conf_data.set('DNAD_NDERIV', dnad_nderiv)

dnad_nderiv_mod = configure_file(input: 'src/dnad_nderiv.f90.in',
                                 output: 'dnad_nderiv.f90',
                                 configuration: dnad_conf_data)

dnad_lib = static_library('dnad', ['src/dnad.f90', dnad_nderiv_mod ])

dnad_unittests = executable('unittests', 'test/unittests.f90', 
                            link_with: dnad_lib)


test('dnad unit tests', dnad_unittests)

