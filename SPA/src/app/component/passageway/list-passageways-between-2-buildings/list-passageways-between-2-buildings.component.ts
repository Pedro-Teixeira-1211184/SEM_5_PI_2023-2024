import {Component, inject, OnInit} from '@angular/core';
import {PassagewayService} from "../../../services/passageway/passageway.service";
import {BuildingService} from "../../../services/building/building.service";
import {FloorService} from "../../../services/floor/floor.service";
import IPassagewayDTO from "../../../dto/IPassagewayDTO";
import IFloorDTO from "../../../dto/IFloorDTO";
import {FormControl, FormGroup, Validators} from "@angular/forms";

@Component({
  selector: 'app-list-passageways-between-2-buildings',
  templateUrl: './list-passageways-between-2-buildings.component.html',
  styleUrls: ['./list-passageways-between-2-buildings.component.css']
})

export class ListPassagewaysBetween2BuildingsComponent implements OnInit {

  service: PassagewayService = inject(PassagewayService);
  b_service: BuildingService = inject(BuildingService);
  f_service: FloorService = inject(FloorService);

  showPassageways: boolean = false;
  passageways: IPassagewayDTO[] = [];
  buildingsCodes: string[] = [];
  floorsCode: string[] = [];

  passagewaysBetween2BuildingsForm!: FormGroup;

  ngOnInit(): void {
    this.passagewaysBetween2BuildingsForm = new FormGroup({
      code1: new FormControl('', [Validators.required]),
      code2: new FormControl('', [Validators.required]),
    });
  }

  constructor() {
    this.getBuildingsCodesForPassageway();
  }

  private async getBuildingsCodesForPassageway() {
    this.buildingsCodes = await this.b_service.getAllBuildingsCode();
  }

  public async submit() {
    if (this.passagewaysBetween2BuildingsForm.valid) {
      this.passageways = await this.service.getPassagewaysInBuildings(this.code1?.value, this.code2?.value) as IPassagewayDTO[];
    }
  }

  get code1() {
    return this.passagewaysBetween2BuildingsForm.get('code1');
  }

  get code2() {
    return this.passagewaysBetween2BuildingsForm.get('code2');
  }

}