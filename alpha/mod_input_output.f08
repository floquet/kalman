!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mInputOutput

    use mFileHandling,                  only : stdout, safeopen_readonly, find_IU_info
    use mIOHandles,                     only : io_handles
    use mKalmanData,                    only : KalmanData

    implicit none

    type ( KalmanData ) :: myData

    integer :: io_handle = stdout

    character ( len = * ), parameter :: PathData = '../input_data/'

    !procedure, public :: start_the_show_sub => start_the_show
    private :: harvest_command_line_sub, open_data_set_sub

CONTAINS

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

    subroutine start_the_show ( )

        ! slot arguments
        type ( io_handles ) :: myIOHandles

            call          open_data_set_sub  ( myIO = myIOHandles )
            call myData % read_file_type_inp ( myIO = myIOHandles )

    end subroutine start_the_show

    !   @   @   @      @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

    subroutine open_data_set_sub ( myIO )

        ! slot arguments
        type ( io_handles ), intent ( out ) :: myIO

        ! local variables
        integer :: FileNameLen
        character ( len = 128 ) :: FileNameStem

            call harvest_command_line_sub ( FileNameStem = FileNameStem, LenCommandArgument  = FileNameLen )

            myIO % inp = safeopen_readonly ( PathData // FileNameStem ( 1 : FileNameLen ) // '.inp' )
            myIO % out = safeopen_readonly ( PathData // FileNameStem ( 1 : FileNameLen ) // '.out' )
            myIO % t   = safeopen_readonly ( PathData // FileNameStem ( 1 : FileNameLen ) // '.t' )
            myIO % p   = safeopen_readonly ( PathData // FileNameStem ( 1 : FileNameLen ) // '.p' )
            myIO % e   = safeopen_readonly ( PathData // FileNameStem ( 1 : FileNameLen ) // '.e' )

    end subroutine open_data_set_sub

    !   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @   @

    subroutine harvest_command_line_sub ( FileNameStem, LenCommandArgument )

        ! slot arguments
        integer, intent ( out ) :: LenCommandArgument
        character ( len = 128 ), intent ( out ) :: FileNameStem

        ! internal variables
        integer :: LenCommandLine
        integer :: GCAStatus, CAStatus

        character ( len = 128 ) :: CommandLine

            write ( io_handle, '( "harvesting command line..." )' )

            ! harvest the launch command for the file name stem
            call get_command_argument ( number = 1, value = FileNameStem, length = LenCommandArgument, status = GCAStatus )
            if ( GCAStatus /= 0 ) then ! failure
                write ( io_handle, 200 )
                write ( io_handle, 210 ) GCAStatus
                call get_command ( command = CommandLine, length = LenCommandLine, status = CAStatus )
                write ( io_handle, 220 ) trim( CommandLine ), LenCommandLine
                stop 'execution halting...'
            end if
            if ( LenCommandArgument == 0 ) then ! no file specified
                write ( io_handle, 230 )
                stop 'Rerun with a file name specified, e.g. ./forsee haa'
            endif

            write ( io_handle, 100 ) trim ( FileNameStem )

            return

            100 format ( 'The command line argument is ', g0, '.' )

            200 format ( /, 'Fatal error in get_command_argument:', /, 'attempting to read first argument' )
            210 format ( 'get_command_argument status = ', g0 )
            220 format ( 'The input command line is ', g0, ', which has length of ', g0, ' characters.')
            230 format ( 'No file name specified in the command line.' )

    end subroutine harvest_command_line_sub

end module mInputOutput
