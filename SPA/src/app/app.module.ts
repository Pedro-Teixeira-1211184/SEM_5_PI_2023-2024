import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';

import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { PageNotFoundComponent } from './page-not-found/page-not-found.component';
import { LoginComponent } from './login/login.component';
import { HomeComponent } from './home/home.component';
import {FormsModule, ReactiveFormsModule} from "@angular/forms";
import { BuildingComponent } from './component/building/building.component';
import { FloorComponent } from './component/floor/floor.component';
import { FloorByBuildingComponent } from './component/floor/floor-by-building/floor-by-building.component';
import { EditBuildingComponent } from './component/building/edit-building/edit-building.component';
import { ListBuildingComponent } from './component/building/list-building/list-building.component';
import {DeactivateRobotComponent} from "./component/robot/deactivate-robot/deactivate-robot.component";
import { CreateRobotTypeComponent } from './component/robot/robot-type/create-robot-type/create-robot-type.component';
import { CreatePassagewayComponent } from './component/passageway/create-passageway/create-passageway.component';
import { PatchMapComponent } from './component/map/patch-map/patch-map.component';
import { EditFloorComponent } from './component/floor/edit-floor/edit-floor.component';
import { ElevatorComponent } from './component/elevator/elevator.component';
import { ListPassagewaysBetween2BuildingsComponent } from './component/passageway/list-passageways-between-2-buildings/list-passageways-between-2-buildings.component';
import { EditPassagewayComponent } from './component/passageway/edit-passageway/edit-passageway.component'; 
import { CreateRobotComponent } from './component/robot/create-robot/create-robot.component';
import { FloorsWithPassagewayComponent } from './component/floor/floors-with-passageway/floors-with-passageway.component';
import { BuildingsByFloorRangeComponent } from './component/building/buildings-by-floor-range/buildings-by-floor-range.component';
import { CreateRoomComponent } from './component/room/create-room/create-room.component';
import { PlaneamentoComponent } from './component/planeamento/planeamento.component';



@NgModule({
    declarations: [
        AppComponent,
        PageNotFoundComponent,
        LoginComponent,
        HomeComponent,
        BuildingComponent,
        FloorComponent,
        FloorByBuildingComponent,
        EditBuildingComponent,
        ListBuildingComponent,
        DeactivateRobotComponent,
        DeactivateRobotComponent,
        CreateRobotTypeComponent,
        CreatePassagewayComponent,
        PatchMapComponent,
        EditFloorComponent,
        ElevatorComponent,
        ListPassagewaysBetween2BuildingsComponent,
        EditPassagewayComponent,
        CreateRobotComponent,
        FloorsWithPassagewayComponent,
        BuildingsByFloorRangeComponent,
<<<<<<< HEAD
        CreateRoomComponent
=======
        PlaneamentoComponent
>>>>>>> 7a1c440497e926fefc3c5d565682f3d59f0850aa
    ],
  imports: [
    BrowserModule,
    AppRoutingModule,
    FormsModule,
    ReactiveFormsModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
