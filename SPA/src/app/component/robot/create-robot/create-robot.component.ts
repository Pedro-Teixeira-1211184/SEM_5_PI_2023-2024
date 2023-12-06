import {Component, inject, OnInit} from '@angular/core';
import {FormControl, FormGroup, Validators} from '@angular/forms';
import {RobotService} from 'src/app/services/robot/robot.service';

@Component({
  selector: 'app-create-robot',
  templateUrl: './create-robot.component.html',
  styleUrls: ['./create-robot.component.css']
})
export class CreateRobotComponent {
  robotForm!: FormGroup;
  service: RobotService = inject(RobotService);

  constructor() {
  }

  public async submit() {
    try {
      const response = await this.service.createRobot(
        this.robotType?.value,
        this.code?.value,
        this.serialNumber?.value,
        this.nickname?.value,
        this.brand?.value,
        this.isActive?.value
      );
    } catch (e) {
      console.log(e);
    }
  }


  ngOnInit(): void {
    this.robotForm = new FormGroup({
      robotType: new FormControl('', [Validators.required]),
      code: new FormControl('', [Validators.required]),
      serialNumber: new FormControl('', [Validators.required]),
      nickname: new FormControl('', [Validators.required]),
      brand: new FormControl('', [Validators.required]),
      isActive: new FormControl('', [Validators.required])
    });
    this.robotForm.get('isActive')?.setValue(false);
  }

  get robotType() {
    return this.robotForm.get('robotType');
  }

  get code() {
    return this.robotForm.get('code');
  }

  get serialNumber() {
    return this.robotForm.get('serialNumber');
  }

  get nickname() {
    return this.robotForm.get('nickname');
  }

  get brand() {
    return this.robotForm.get('brand');
  }

  get isActive() {
    return this.robotForm.get('isActive');
  }
}
