import {Component, inject, OnInit} from '@angular/core';
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {FloorService} from "../../services/floor/floor.service";
import {BuildingService} from "../../services/building/building.service";


@Component({
  selector: 'app-floor',
  templateUrl: './floor.component.html',
  styleUrls: ['./floor.component.css']
})
export class FloorComponent implements OnInit {
  floorForm!: FormGroup;
  service: FloorService = inject(FloorService);
  b_service: BuildingService = inject(BuildingService);
  buildings: string[] = [];

  constructor() {
    this.getAllBuildings();
  }

  public async submit() {
    try{
      if (this.floorForm.invalid) {
        alert('Please fill all the fields');
        return;
      }
      const response = await this.service.createFloor(
        this.buildingCode?.value,
        this.number?.value,
        this.description?.value
      );
    }catch(e){
      console.log(e);
    }
  }

  ngOnInit(): void {
    this.floorForm = new FormGroup({
      buildingCode: new FormControl('', [Validators.required]),
      number: new FormControl('', [Validators.required]),
      description: new FormControl()
    });
  }

  public async getAllBuildings(){
    this.buildings = await this.b_service.getAllBuildingsCode();
  }

  get buildingCode() {
    return this.floorForm.get('buildingCode');
  }

  get number() {
    return this.floorForm.get('number');
  }

  get description() {
    return this.floorForm.get('description');
  }

}
