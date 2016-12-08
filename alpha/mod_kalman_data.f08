!23456789 123456789 223456789 323456789 423456789 523456789 623456789 723456789 823456789 923456789 023456789 123456789 223456789 32
module mKalmanData
    implicit none

    type :: KalmanData
        character ( len = 128 ) :: title
    CONTAINS
        private
        procedure, public :: read_file_type_inp_sub  =>  read_file_type_inp
    end type KalmanData

contains

    read_file_type_inp_sub ()
        class ( KalmanData ), target :: me
        me % title = 'title'
    end read_file_type_inp_sub

end module mKalmanData
