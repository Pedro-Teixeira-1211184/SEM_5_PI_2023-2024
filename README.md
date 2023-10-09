<img style="float: right; bottom:10pc" src="/RobDroneGo/src/loaders/_images/logo_isep.png"/>

#  <span style="color: red; text-decoration: underline">RobDroneGo</span>
## Main goal
ISEP Integrating project of all the curricular units (CU) of the 5th semester, being carried out during the semester simultaneously by all the curricular units: ALGAV, ARQSI, ASIST, SGRAI.

The aim is to develop a prototype for a task execution system for a fleet of robots and drones.
The system will be called <u>RobDroneGo</u>.
The system should consist of the following modules:
- Device management
- Task request management
- Task execution planning

## Overview
ISEP intends to develop a solution for managing a fleet of drones and robots that can perform tasks inside its campus.
Consider that ISEP acquires robots of two types:
### Robisep 
A mobile robot that moves using a wheel system and can in building corridors or by elevators between floors of a building, but cannot climb stairs. The robot can move from one building to another, but only through the covered corridors connecting buildings. Robiseps can have installed systems that carry out various tasks, such as surveillance or cleaning the corridor or access to a room/office to pick up/deliver an item (e.g. a projector remote control).

### Droneisep 
A drone that moves around outside ISEP buildings.
The droneisep can go from one point to another point in space by moving along permitted straight-line paths. The tasks of these drones can be various, such as delivering objects, surveillance, image acquisition, exterior window cleaning operations, etc.
<br/>
There will be the following types of system users:
- System administrator - manages users and their authorizations
- Fleet manager - manages robot and drone data and types of tasks
- Campus manager - manages route data and maps
- User (student, teacher, employee) - requests the execution of tasks
<br/>

Initially, the approval of task requests and their scheduling will be carried out manually by the Task Manager, but may in the future evolve into an automatic system. In order to execute the request, the system will plan the route that the device (drone or robot) must take.

In addition to the issue of people's safety (collisions between the robot and people, or, more critically, if the robot is critical, if the robot is taken with bad intentions to try to hurt people) there is also the question of data protection, for example in surveillance tasks that will use image acquired by the camera(s). Also fundamental is preventing the robot from running over people. So each robot will have a set of sensors that will allow it to navigate locally while avoiding obstacles.
For the purposes of prototyping, average navigation times will be used without unpredictability of obstacle avoidance, nor will aspects of the robot's local control.
For demonstration purposes, the solution is expected to be demonstrated with a campus of at least 5 buildings, with at least one connection to another building allowing circulation between all the buildings on campus, with each floor having 3 to 5 floors and an average of 10 offices/rooms per floor.

## Group Members

The members of the group:

| Student Nr.	                     | Name			               |
|----------------------------------|-----------------------|
| **[1211184](1211184/README.md)** | Pedro Teixeira        |
| **[1211188](1211188/readme.md)** | Tomás Oliveira						  |
| **[1211173](1211221/readme.md)** | Leonor Curado	       |

<br/>

## GitHUB Repository
https://github.com/Pedro-Teixeira-1211184/SEM_5_PI_2023-2024