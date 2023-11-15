import {Component, inject, OnInit} from '@angular/core';
import {PassagewayService} from "../../../services/passageway/passageway.service";
import {BuildingService} from "../../../services/building/building.service";
import {FloorService} from "../../../services/floor/floor.service";
import {FormControl, FormGroup, Validators} from "@angular/forms";

@Component({
  selector: 'app-create-passageway',
  templateUrl: './create-passageway.component.html',
  styleUrls: ['./create-passageway.component.css']
})
export class CreatePassagewayComponent implements OnInit {

  service: PassagewayService = inject(PassagewayService);
  b_service: BuildingService = inject(BuildingService);
  f_service: FloorService = inject(FloorService);

  buildingsCodes: string[] = [];
  floorsCode: string[] = [];

  passagewayForm!: FormGroup;

  ngOnInit(): void {
    this.passagewayForm = new FormGroup({
      code1: new FormControl('', [Validators.required]),
      code2: new FormControl('', [Validators.required]),
    });
  }

  constructor() {
    this.getBuildingsCodes();
  }

  private async getBuildingsCodes() {
    this.buildingsCodes = await this.b_service.getAllBuildingsCode();
    for (let buildingCode of this.buildingsCodes) {
      const x = await this.f_service.getFloorsByBuildingCodeForPassageway(buildingCode);
      for (let floor of x) {
        this.floorsCode.push(floor.code);
      }
    }
  }

  public async submit() {
    if (this.passagewayForm.valid) {
      await this.service.createPassageway(this.code1?.value, this.code2?.value);
    }
  }

  get code1() {
    return this.passagewayForm.get('code1');
  }

  get code2() {
    return this.passagewayForm.get('code2');
  }

}
