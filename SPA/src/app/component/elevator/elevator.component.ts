import { Component, inject, OnInit } from '@angular/core';
import { FormControl, FormGroup, Validators } from "@angular/forms";
import { ElevatorService } from "../../services/elevator/elevator.service";

@Component({
  selector: 'app-elevator',
  templateUrl: './elevator.component.html',
  styleUrls: ['./elevator.component.css']
})
export class ElevatorComponent {
  elevatorForm!: FormGroup;
  service: ElevatorService = inject(ElevatorService);

  constructor() {
  }

  public async submit(){
    try{
      if(this.elevatorForm.invalid){
        alert('Please fill all the fields');
        return;
      }
      const response = await this.service.createElevator(
        this.coordenates?.value,
        this.buildingCode?.value,
        this.floorNumbers?.value
      );
    }catch (e) {
      console.log(e);
    }
  }

  ngOnInit(): void {
    this.elevatorForm = new FormGroup({
      coordenates: new FormControl('', [Validators.required]),
      buildingCode: new FormControl('', [Validators.required]),
      floorNumbers: new FormControl('', [Validators.required])
    });
  }

  get coordenates() {
    return this.elevatorForm.get('coordenates');
  }

  get buildingCode() {
    return this.elevatorForm.get('buildingCode');
  }

  get floorNumbers() {
    return this.elevatorForm.get('floorNumbers');
  }

}
