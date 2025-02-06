# uic-psy-schedule
schedule residents for UIC psychiatry program

## create call schedule

* [x] put dummy data of resident night float, leave, call requests into 20200701_fall.csv
  * [ ] create form that residents can input their preferences, outputs in same format as .csv
* [ ] create empty calendar of all call shifts to be covered in Fall 2024 (each row is date)
  * [x] UIC schedule
  * [ ] JBVA schedule
* [ ] create tidy dataset of all shifts to be covered in Fall 2024 (each row is shift)
  * [x] UIC shifts
  * [ ] JBVA shifts
* [ ] schedule residents to night float and leave by joining 20240701_fall.csv to shifts_empty.csv
* [ ] find algorithms/solvers to assign residents to remaining shifts
* [X] write hard and soft constraints in English
* [ ] code hard and soft constraints into logic algorithm can understand
* [ ] create PDF reports for each resident showing their total hours:
  * [ ] for the semester
  * [ ] cumulative for entire residency program
* [ ] create slides presentation illustrating the proof of concept
* [ ] learn about additional hard and soft constraints from administrators

## future work

* [ ] create annual holiday call schedule
* [ ] create annual rotation schedule

## references

* [Nurse scheduling problem](https://en.wikipedia.org/w/index.php?title=Nurse_scheduling_problem&oldid=1260095526) Wikipedia
* Raymond Hettinger [Modern solvers: Problems well-defined are problems solved](https://www.youtube.com/watch?v=_GP9OpZPUYc) PyCon 2019
* Kim V (2024) [lpsymphony: Symphony integer linear programming solver in R. R package version 1.34.0](https://bioconductor.org/packages/release/bioc/html/lpsymphony.html).
* [Employee Scheduling | OR-Tools](https://developers.google.com/optimization/scheduling/employee_scheduling) Google for Developers
* u/eveeyelaw [Creating a complex staff schedule for a hospital department.](www.reddit.com/r/learnpython/comments/pevtm7/creating_a_complex_staff_schedule_for_a_hospital/) reddit.com/r/learnpython
