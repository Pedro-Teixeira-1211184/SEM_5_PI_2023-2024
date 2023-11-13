import { Component, inject, OnInit } from '@angular/core';
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {FloorService} from "../../../services/floor/floor.service";
import { map } from 'rxjs/operators';

@Component({
  selector: 'app-floor-by-building',
  templateUrl: './floor-by-building.component.html',
  styleUrls: ['./floor-by-building.component.css']
})
export class FloorByBuildingComponent implements OnInit{
  floorForm!: FormGroup;
  service: FloorService = inject(FloorService);
  floors: any[] = [];

  constructor() { }

  public async getFloorsByBuildingCode(){
    try{
      console.log(this.buildingCode?.value);
      if (this.buildingCode?.value === '') {
        alert('Please choose a building');
        return;
      }
      this.floors = await this.service.getFloorsByBuildingCode(this.buildingCode?.value).pipe(map((res: any) => res.data)).toPromise();
    }catch(e){
      console.log(e);
    }
  }

  ngOnInit(): void {
    this.floorForm = new FormGroup({
      buildingCode: new FormControl('', [Validators.required])
    });
  }

  get buildingCode() {
    return this.floorForm.get('buildingCode');
  }
}
