import { Component } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { FloorService } from 'src/app/services/floor/floor.service';
import { OnInit } from '@angular/core';

@Component({
  selector: 'app-edit-floor',
  templateUrl: './edit-floor.component.html',
  styleUrls: ['./edit-floor.component.css']
})
export class EditFloorComponent implements OnInit{
    floorEditForm!: FormGroup;
    service: FloorService = new FloorService();

  constructor() { 
  }
  ngOnInit(): void {
    this.floorEditForm = new FormGroup({
      buildingCode: new FormControl('', [Validators.required]),
      floorNumber: new FormControl('', [Validators.required]),
      description: new FormControl('', [Validators.required])
    });
  }

    public async submit() {
      try {
        if (this.floorEditForm.invalid) {
          alert('Please fill all the fields');
          return;
        }
        const response = await this.service.editFloor(
          this.buildingCode?.value,
          this.floorNumber?.value,
          this.description?.value
        );
      } catch (e) {
        console.log(e);
      }
    }

    get buildingCode() {
      return this.floorEditForm.get('buildingCode');
    }

    get floorNumber() {
      return this.floorEditForm.get('floorNumber');
    }

    get description() {
      return this.floorEditForm.get('description');
    }

    
}