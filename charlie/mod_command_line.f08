!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mCommandLine

    use mConstants,                     only : stdout, fmt_generic

    implicit none

    integer :: io_handle = stdout

contains
    ! harvest_command_line_sub

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

    subroutine harvest_command_line_sub ( FileNameStem, LenCommandArgument )

        ! slot arguments
        integer,                 intent ( out ) :: LenCommandArgument
        character ( len = 128 ), intent ( out ) :: FileNameStem

        ! internal variables
        integer :: LenCommandLine
        integer :: GCAStatus, CAStatus

        character ( len = 128 ) :: CommandLine

            ! harvest the launch command for the file name stem
            call get_command_argument ( number = 1, value = FileNameStem, length = LenCommandArgument, status = GCAStatus )
            if ( GCAStatus /= 0 ) then ! failure
                write ( io_handle, 200 )
                write ( io_handle, fmt_generic ) 'get_command_argument status = ', GCAStatus
                call get_command ( command = CommandLine, length = LenCommandLine, status = CAStatus )
                write ( io_handle, fmt_generic ) 'The input command line is ', trim( CommandLine ), &
                                                 ', which has length of ', LenCommandLine, ' characters.'
                stop 'execution halting...'
            end if
            if ( LenCommandArgument == 0 ) then ! no file specified
                write ( io_handle, fmt_generic ) 'No file name specified in the command line.'
                stop 'Rerun with a file name specified, e.g. ./forsee haa'
            endif

            return

            200 format ( /, 'Fatal error in get_command_argument:', /, 'attempting to read first argument' )

    end subroutine harvest_command_line_sub

end module mCommandLine
