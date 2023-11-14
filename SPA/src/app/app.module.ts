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
        DeactivateRobotComponent
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
