import {Component, inject, OnInit} from '@angular/core';
import {FloorService} from "../../../services/floor/floor.service";
import {BuildingService} from "../../../services/building/building.service";
import {MapService} from "../../../services/map/map.service";
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {PassagewayService} from "../../../services/passageway/passageway.service";
import IFloorDTO from "../../floor/dto/IFloorDTO";
import IBuildingDTO from "../../building/list-building/dto/IBuildingDTO";

@Component({
  selector: 'app-patch-map',
  templateUrl: './patch-map.component.html',
  styleUrls: ['./patch-map.component.css']
})
export class PatchMapComponent implements OnInit {

  service = inject(MapService);
  b_service = inject(BuildingService);
  p_service = inject(PassagewayService);
  f_service = inject(FloorService);

  mapForm!: FormGroup;

  buildings: IBuildingDTO[] = [];
  floors: IFloorDTO[] = [];

  matrix: number[][] = [];
  columns: number = 3;
  rows: number = 3;

  constructor() {
    this.getBuildingsCodes();
  }

  ngOnInit(): void {
    this.mapForm = new FormGroup({
      buildingCode: new FormControl('', [Validators.required]),
      floorCode: new FormControl('', [Validators.required]),
    });
    this.initializeMatrix();
  }

  initializeMatrix() {
    this.matrix = Array.from({length: this.columns}, () =>
      Array.from({length: this.rows}, () => 0)
    );
  }

  private async getBuildingsCodes() {
    this.buildings = await this.b_service.getAllBuildings();
  }

  private async getFloorsCodes(buildingCode: string) {
    this.floors = [];
    this.floors = await this.f_service.getFloorsByBuildingCodeForPassageway(buildingCode);
  }

  get buildingCode() {
    return this.mapForm.get('buildingCode');
  }

  get floorCode() {
    return this.mapForm.get('floorCode');
  }

  public async submit() {
    //TODO: submit
  }

  public async getFloorsOfBuilding() {
    //set grid dimensions
    for (let i = 0; i < this.buildings.length; i++) {
      if (this.buildings[i].code == this.buildingCode?.value) {
        this.columns = this.buildings[i].dimensions.length + 1;
        this.rows = this.buildings[i].dimensions.width + 1;
        break;
      }
    }
    this.initializeMatrix();
    await this.getFloorsCodes(this.buildingCode?.value);
  }
}
