import { Component } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { FloorService } from 'src/app/services/floor/floor.service';

@Component({
  selector: 'app-edit-floor',
  templateUrl: './edit-floor.component.html',
  styleUrls: ['./edit-floor.component.css']
})
export class EditFloorComponent {
    floorForm!: FormGroup;
    service: FloorService = new FloorService();

  constructor() { 
  }


  ngonInit(): void {
    this.floorForm = new FormGroup({
      buildingCode: new FormControl('', [Validators.required]),
      number: new FormControl('', [Validators.required]),
      code: new FormControl('', [Validators.required]),
      description: new FormControl('', [Validators.required])
    });
  }

    public async submit() {
      try {
        if (this.floorForm.invalid) {
          alert('Please fill all the fields');
          return;
        }
        const response = await this.service.editFloor(
          this.buildingCode?.value,
          this.number?.value,
          this.code?.value,
          this.description?.value
        );
      } catch (e) {
        console.log(e);
      }
    }

    get buildingCode() {
      return this.floorForm.get('buildingCode');
    }

    get number() {
      return this.floorForm.get('number');
    }

    get code() {
      return this.floorForm.get('code');
    }

    get description() {
      return this.floorForm.get('description');
    }

    
}