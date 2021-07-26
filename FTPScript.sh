#!/bin/bash
/usr/local/bin/ftp -d ftp.backwelltennisclub.co.uk << ftpEOF
   prompt
   put "Entries_8_and_under_singles_for_boys_and_girls.pdf"
   put "Entries_9_and_under_singles_for_boys_and_girls.pdf"
   put "Entries_10_and_under_singles_for_boys_and_girls.pdf"
   put "Entries_11_and_under_doubles_for_boys_and_girls.pdf"
   put "Entries_11_and_under_singles_for_boys_and_girls.pdf"
   put "Entries_12_and_under_doubles_for_boys_and_girls.pdf"
   put "Entries_12_and_under_singles_for_boys_and_girls.pdf"
   put "Entries_14_and_under_boys_doubles.pdf"
   put "Entries_14_and_under_boys_singles.pdf"
   put "Entries_14_and_under_girls_singles.pdf"
   put "Entries_16_and_under_boys_singles.pdf"
   put "Entries_16_and_under_girls_doubles.pdf"
   put "Entries_16_and_under_girls_singles.pdf"
   put "Entries_16_and_under_mixed_doubles.pdf"
   put "Entries_50_and_over_men's_doubles.pdf"
   put "Entries_50_and_over_mixed_doubles.pdf"
   put "Entries_50_and_over_women's_doubles.pdf"
   put "Entries_Men's_doubles.pdf"
   put "Entries_Men's_singles.pdf"
   put "Entries_Men's_singles_plate.pdf"
   put "Entries_Mixed_doubles.pdf"
   put "Entries_Men's_doubles_plate.pdf"
   put "Entries_Women's_doubles.pdf"
   put "Entries_Women's_singles.pdf"
   quit
ftpEOF
