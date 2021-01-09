(kcombine
 (output "board1.kicad_sch")
 (sheet (sch_file "led/led.kicad_sch")
	(git_tag "HEAD")
	(local_name "myled")
	(inst "led1"))
 )

