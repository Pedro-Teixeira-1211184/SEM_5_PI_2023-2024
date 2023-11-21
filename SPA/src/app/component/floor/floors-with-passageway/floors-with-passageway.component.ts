import { Component,inject, OnInit } from '@angular/core';
import {FormControl, FormGroup, Validators} from "@angular/forms";
import { BuildingService } from 'src/app/services/building/building.service';
import { FloorService } from 'src/app/services/floor/floor.service';


@Component({
  selector: 'app-floors-with-passageway',
  templateUrl: './floors-with-passageway.component.html',
  styleUrls: ['./floors-with-passageway.component.css']
})

export class FloorsWithPassagewayComponent implements OnInit {
  floorsWithPassagewayForm!: FormGroup;
  service: FloorService = inject(FloorService);
  b_service: BuildingService = inject(BuildingService);
  allBuildingsCodes: string[] = [];
  results: string[] = [];
  showResults: boolean = false;
  
  constructor() {
    this.getBuildingsCodesForPassageway();
  }

  public async getBuildingsCodesForPassageway() {
    this.allBuildingsCodes = await this.b_service.getAllBuildingsCode();
  }
  
    ngOnInit(): void {
      this.floorsWithPassagewayForm = new FormGroup({
        buildingCode: new FormControl('', [Validators.required])
      });
    }

    public async submit(){
      this.showResults = true;
      this.results = [];
      if (this.floorsWithPassagewayForm.valid) {
        this.results = await this.service.getFloorsWithPassageways(this.buildingCode?.value);
      }
    } 

    get buildingCode() {
      return this.floorsWithPassagewayForm.get('buildingCode');
    }

}
