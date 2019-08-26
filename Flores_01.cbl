        IDENTIFICATION DIVISION.
        PROGRAM-ID. Flores_01.

        DATA DIVISION.
            WORKING-STORAGE SECTION.
            77 CHOICE PIC 9.
            77 EXITED PIC 9 VALUE 0.
            77 SEARCHNUM PIC X(30).
            77 j PIC 9.


            01 students.
                02 student-info occurs 5 times indexed by i.
                    03 fullname PIC X(30).
                    03 sNo PIC X(30).
                    03 course PIC X(20).
                    03 contact-info.
                        04 mobile PIC X(11).
                        04 landline PIC X(8).
                    03 age PIC 99.

        PROCEDURE DIVISION.

        PERFORM MENU UNTIL EXITED = 1.
        STOP RUN.

        MENU.
            DISPLAY "  ====MENU===="
            DISPLAY "[1] Add Student"
            DISPLAY "[2] Edit Student"
            DISPLAY "[3] Delete Student"
            DISPLAY "[4] View Student"
            DISPLAY "[5] View All Student"
            DISPLAY "[0] Exit"
            DISPLAY " "
            DISPLAY " "
            DISPLAY "Choice: " WITH NO ADVANCING.
            ACCEPT CHOICE
            IF CHOICE > 5 
                PERFORM MENU
            END-IF.
           


        IF CHOICE >= 0 AND CHOICE < 6
            *> exit condition
            IF CHOICE = 0
               COMPUTE EXITED = 1
            END-IF

            IF CHOICE = 1 *> FUNCTIONAL
                IF i > 5
                    DISPLAY "ERROR! Directory is full." *> PRINTS AN ERROR WHEN NUMBER OF STUDENTS EXCEED 5
                ELSE 
                    DISPLAY " "
                    DISPLAY "  ====ADD A STUDENT===="
                    DISPLAY "ENTER FULLNAME: " WITH NO ADVANCING
                    ACCEPT fullname(i)
                    DISPLAY "ENTER STUDENT NUMBER: " WITH NO ADVANCING
                    ACCEPT sNo(i)
                    DISPLAY "ENTER AGE: " WITH NO ADVANCING
                    ACCEPT age(i)
                    DISPLAY "ENTER DEGREE PROGRAM: " WITH NO ADVANCING
                    ACCEPT course(i)
                    DISPLAY "ENTER MOBILE NUMBER: " WITH NO ADVANCING
                    ACCEPT mobile(i)
                    DISPLAY "ENTER LANDLINE NUMBER: " WITH NO ADVANCING
                    ACCEPT landline(i)

                    COMPUTE i = i + 1 *> INCREMENTS index by 1 for next insert 
                    

                END-IF

            END-IF
            
            IF CHOICE = 2 *> FUNCTIONAL
                DISPLAY "  ====EDIT STUDENT INFO===="
                DISPLAY "ENTER STUDENT NUMBER: " WITH NO ADVANCING
                ACCEPT SEARCHNUM
                
                SET i TO 1 
                SEARCH student-info *> search function 
                    AT END DISPLAY "Student not found." *>displays an error message when student number isnt found in the table
                    WHEN sNo(i) = SEARCHNUM
                    DISPLAY "STUDENT FOUND."
                    DISPLAY "ENTER AGE: " WITH NO ADVANCING 
                    ACCEPT age(i)
                    DISPLAY "ENTER COURSE: " WITH NO ADVANCING 
                    ACCEPT course(i)
                    DISPLAY "ENTER MOBILE No.: " WITH NO ADVANCING
                    ACCEPT mobile(i)
                    DISPLAY "ENTER LANDLINE No.:" WITH NO ADVANCING
                    ACCEPT landline(i)
                END-SEARCH

            END-IF
            
            IF CHOICE = 3 *>FUNCTIONAL
                DISPLAY "  ====DELETE STUDENT===="
                DISPLAY "ENTER STUDENT NUMBER: " WITH NO ADVANCING
                ACCEPT SEARCHNUM

                SET i TO 1
                SEARCH student-info
                    AT END DISPLAY "Student not found."
                    WHEN sNo(i) = SEARCHNUM
                    DISPLAY "STUDENT FOUND."
                    SET fullname(i) to "" *>sets everything to "null"
                    SET sNo(i) to ""
                    SET course(i) to ""
                    SET mobile(i) to ""
                    SET landline(i) to ""
                    SET age(i) to ""

                    SET j TO i *> j=i so that the next insert of student will be on the deleted

                END-SEARCH
                SET i TO j

            END-IF

            IF CHOICE = 4 *>FUNCTIONAL
                DISPLAY "  ====VIEW STUDENT===="
                DISPLAY "ENTER STUDENT NUMBER: " WITH NO ADVANCING
                ACCEPT SEARCHNUM

                SET i TO 1
                SEARCH student-info
                    AT END DISPLAY "Student not found."
                    WHEN sNo(i) = SEARCHNUM
                    DISPLAY "=================="
                    DISPLAY "|" sNo(i)
                    DISPLAY "|" fullname(i)
                    DISPLAY "|" age(i)
                    DISPLAY "|" course(i)
                    DISPLAY "|" mobile(i)
                    DISPLAY "|" landline(i)
                    DISPLAY "=================="
                END-SEARCH


            END-IF

            IF CHOICE = 5
                DISPLAY "  ====VIEW ALL STUDENTS===="
                PERFORM DISPLAY-ALL VARYING j FROM 1 BY 1 UNTIL j>5.
                DISPLAY-ALL.
                    DISPLAY "=================="
                    DISPLAY "|" sNo(j)
                    DISPLAY "|" fullname(j)
                    DISPLAY "|" age(j)
                    DISPLAY "|" course(j)
                    DISPLAY "|" mobile(j)
                    DISPLAY "|" landline(j)
                    DISPLAY "==================".
                


        