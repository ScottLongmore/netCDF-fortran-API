
      program netcdfai_test_driv 
      ! test nf90api and HURSAT algorithm modules
        use fruit
        use netcdfai_test

        call init_fruit 

        call test_nf90ai

        call fruit_summary

      end program netcdfai_test_driv
