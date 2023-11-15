import {Component, inject, OnInit} from '@angular/core';
import {FloorService} from "../../../services/floor/floor.service";
import {BuildingService} from "../../../services/building/building.service";
import {MapService} from "../../../services/map/map.service";
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {PassagewayService} from "../../../services/passageway/passageway.service";

@Component({
  selector: 'app-patch-map',
  templateUrl: './patch-map.component.html',
  styleUrls: ['./patch-map.component.css']
})
export class PatchMapComponent implements OnInit {

  service = inject(MapService);
  b_service = inject(BuildingService);
  p_service = inject(PassagewayService);

  mapForm!: FormGroup;

  buildingsCodes: string[] = [];

  matrix: number[][] = [];
  columns: number = 3;
  rows: number = 3;

  constructor() {
    this.getBuildingsCodes();
  }

  ngOnInit(): void {
    this.mapForm = new FormGroup({
      buildingCode: new FormControl('', [Validators.required]),
      floorNumber: new FormControl('', [Validators.required]),
      length: new FormControl('', [Validators.required]),
      width: new FormControl('', [Validators.required]),
    });
    this.initializeMatrix();
  }

  initializeMatrix() {
    this.matrix = Array.from({ length: this.columns }, () =>
      Array.from({ length: this.rows }, () => 0)
    );
  }

  private async getBuildingsCodes() {
    this.buildingsCodes = await this.b_service.getAllBuildingsCode();
  }

  get buildingCode() {
    return this.mapForm.get('buildingCode');
  }

  get floorNumber() {
    return this.mapForm.get('floorNumber');
  }

  get length() {
    return this.mapForm.get('length');
  }

  get width() {
    return this.mapForm.get('width');
  }

  public async submit() {
    console.log(this.columns);
    console.log(this.rows);
  }

  confirmLength() {
    this.columns = this.length?.value;
    this.initializeMatrix();
  }

  confirmWidth() {
    this.rows = this.width?.value;
    this.initializeMatrix();
  }
}
