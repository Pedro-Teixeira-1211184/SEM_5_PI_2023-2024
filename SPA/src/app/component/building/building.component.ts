import {Component, inject, OnInit} from '@angular/core';
import {FormControl, FormGroup, Validators} from "@angular/forms";
import {BuildingService} from "../../services/building/building.service";

@Component({
  selector: 'app-building',
  templateUrl: './building.component.html',
  styleUrls: ['./building.component.css']
})
export class BuildingComponent implements OnInit {
  buildingForm!: FormGroup;
  service: BuildingService = inject(BuildingService);

  constructor() {
  }

  public async submit() {
    try {
      if (this.buildingForm.invalid) {
        alert('Please fill all the fields');
        return;
      }
      const response = await this.service.createBuilding(
        this.code?.value,
        this.length?.value,
        this.width?.value,
        this.name?.value,
        this.description?.value,
        this.maxFloors?.value,
        this.minFloors?.value
      );
    } catch (e) {
      console.log(e);
    }
  }

  ngOnInit(): void {
    this.buildingForm = new FormGroup({
      code: new FormControl('', [Validators.required]),
      length: new FormControl('', [Validators.required]),
      width: new FormControl('', [Validators.required]),
      name: new FormControl(),
      description: new FormControl(),
      maxFloors: new FormControl('', [Validators.required]),
      minFloors: new FormControl('', [Validators.required])
    });
  }

  get code() {
    return this.buildingForm.get('code');
  }

  get length() {
    return this.buildingForm.get('length');
  }

  get width() {
    return this.buildingForm.get('width');
  }

  get name() {
    return this.buildingForm.get('name');
  }

  get description() {
    return this.buildingForm.get('description');
  }

  get maxFloors() {
    return this.buildingForm.get('maxFloors');
  }

  get minFloors() {
    return this.buildingForm.get('minFloors');
  }
}
