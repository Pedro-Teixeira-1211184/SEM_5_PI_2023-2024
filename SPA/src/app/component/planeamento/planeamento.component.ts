import { Component, inject, OnInit } from '@angular/core';
import {FormControl, FormGroup, Validators} from "@angular/forms";
import { MapService } from '../../services/map/map.service';
import IFloorDTO from '../../dto/IFloorDTO';
import { FloorService } from 'src/app/services/floor/floor.service';
import IPlantDTO from '../../dto/IPlantDTO';

@Component({
  selector: 'app-planeamento',
  templateUrl: './planeamento.component.html',
  styleUrls: ['./planeamento.component.css']
})
export class PlaneamentoComponent {
  planeamentoForm!: FormGroup;
  mapservice: MapService = inject(MapService);
  floorservice: FloorService = inject(FloorService);
  floors: IFloorDTO[] = [];
  result: any;

  constructor() {

  }

  public async getFloorsByBuildingCode(){
    if(this.buildingCode1?.value === '' || this.buildingCode1?.value === null || this.buildingCode1?.value === undefined){
      alert ('Please choose a building');
      return;
    }
    this.floors = await this.floorservice.getFloorsByBuildingCode(this.buildingCode1?.value) as IFloorDTO[];
  }

  public async getFloorsByBuildingCode2(){
    if(this.buildingCode2?.value === '' || this.buildingCode2?.value === null || this.buildingCode2?.value === undefined){
      alert ('Please choose a building');
      return;
    }
    this.floors = await this.floorservice.getFloorsByBuildingCode(this.buildingCode2?.value) as IFloorDTO[];
  }


  public async chooseFloor1(){
    if(this.origin?.value === '' || this.origin?.value === null || this.origin?.value === undefined){
      alert ('Please insert a floor number');
      return;
    }
  }

  public async chooseFloor2(){
    if(this.destination?.value === '' || this.destination?.value === null || this.destination?.value === undefined){
      alert ('Please insert a floor number');
      return;
    }
  }

  public async definePath() {
    // Get the values of the origin and destination form controls
    const originValue = this.planeamentoForm.get('buildingCode1')?.value + this.planeamentoForm.get('origin')?.value;
    const destinationValue = this.planeamentoForm.get('buildingCode2')?.value + this.planeamentoForm.get('destination')?.value;
  
    // Log the values to the console
    console.log('Origin:', originValue);
    console.log('Destination:', destinationValue);
  
    if (originValue == destinationValue) {
      alert('The origin and destination are the same');
      return;
    }
  
    // Call the pathBetweenFloors method with the origin and destination values
    this.result = await this.mapservice.pathBetweenFloors(originValue, destinationValue);
    console.log(this.result);
  }
  

  ngOnInit(): void {
    this.planeamentoForm = new FormGroup({
      buildingCode1: new FormControl('', [Validators.required]),
      origin: new FormControl('', [Validators.required]),
      buildingCode2: new FormControl('', [Validators.required]),
      destination: new FormControl('', [Validators.required]),
    });
  }
  
    get buildingCode1() {
      return this.planeamentoForm.get('buildingCode1');
    }

    get origin() {
      return this.planeamentoForm.get('origin');
    }

    get buildingCode2() {
      return this.planeamentoForm.get('buildingCode2');
    }

    get destination() {
      return this.planeamentoForm.get('destination');
    }
  }
