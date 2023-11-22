import { Component, inject, OnInit } from '@angular/core';
import {FormControl, FormGroup, Validators} from "@angular/forms";
import { MapService } from 'src/app/services/map/map.service';
import IFloorDTO from '../../dto/IFloorDTO';
import { FloorService } from 'src/app/services/floor/floor.service';


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
  origin: string = '';
  destination: string = '';

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

  public async loadMapOrigin(){
    if(this.floorNumber1?.value === '' || this.floorNumber1?.value === null || this.floorNumber1?.value === undefined){
      alert ('Please choose a floor');
      return;
    }
    this.mapservice.loadMap(this.buildingCode1?.value, this.floorNumber1?.value);
  }

  public async loadMapDestination(){
    if(this.floorNumber2?.value === '' || this.floorNumber2?.value === null || this.floorNumber2?.value === undefined){
      alert ('Please choose a floor');
      return;
    }
    this.mapservice.loadMap(this.buildingCode2?.value, this.floorNumber2?.value);
  }

  public async chooseOrigin(){
    if(this.origin === '' || this.origin === null || this.origin === undefined){
      alert ('Please choose the coordinates for the origin (x,y)');
      return;
    }
  }

  public async chooseDestination(){
    if(this.destination === '' || this.destination === null || this.destination === undefined){
      alert ('Please choose the coordinates for the destination (x,y)');
      return;
    }
  }

  public async definePath(){
    if(this.origin === this.destination){
      alert ('The origin and destination are the same');
      return;
    }
    this.mapservice.pathBetweenFloors(this.origin, this.destination);
  }


  ngOnInit(): void {
    this.planeamentoForm = new FormGroup({
      buildingCode1: new FormControl('', [Validators.required]),
      floorNumber1: new FormControl('', [Validators.required]),
      origin: new FormControl('', [Validators.required]),
      buildingCode2: new FormControl('', [Validators.required]),
      floorNumber2: new FormControl('', [Validators.required]),
      destination: new FormControl('', [Validators.required])
    });
  }
    get buildingCode1() {
      return this.planeamentoForm.get('buildingCode1');
    }

    get floorNumber1() {
      return this.planeamentoForm.get('floorNumber1');
    }

    get buildingCode2() {
      return this.planeamentoForm.get('buildingCode2');
    }

    get floorNumber2() {
      return this.planeamentoForm.get('floorNumber2');
    }
  }
