import { Component, inject, OnInit } from '@angular/core';
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {FloorService} from "../../../services/floor/floor.service";
import { map } from 'rxjs/operators';
import IFloorDTO from '../../../dto/IFloorDTO';

@Component({
  selector: 'app-floor-by-building',
  templateUrl: './floor-by-building.component.html',
  styleUrls: ['./floor-by-building.component.css']
})
export class FloorByBuildingComponent implements OnInit{
  floorForm!: FormGroup;
  service: FloorService = inject(FloorService);
  floors: IFloorDTO[] = [];
  Object = Object;

  constructor() {
  }

  public async getFloorsByBuildingCode(){
    if(this.code?.value === '' || this.code?.value === null || this.code?.value === undefined){
      alert ('Please choose a building');
      return;
    }
    this.floors = await this.service.getFloorsByBuildingCode(this.code?.value) as IFloorDTO[];
  }


  ngOnInit(): void {
    this.floorForm = new FormGroup({
      code: new FormControl('', [Validators.required])
    });
  }

  get code() {
    return this.floorForm.get('code');
  }
}
