import {Component, inject, OnInit} from '@angular/core';
import {BuildingService} from "../../../services/building/building.service";
import IBuildingDTO from "./dto/IBuildingDTO";

@Component({
  selector: 'app-list-building',
  templateUrl: './list-building.component.html',
  styleUrls: ['./list-building.component.css']
})
export class ListBuildingComponent implements OnInit{
  service: BuildingService = inject(BuildingService);
  buildings: IBuildingDTO[] = [];

    constructor() {
        this.getAllBuildings();
    }

    ngOnInit(): void {
    }

  public async getAllBuildings(){
    this.buildings = await this.service.getAllBuildings();
  }

}
