import { Component } from '@angular/core';
import { FormControl, FormGroup, Validators } from '@angular/forms';
import { PassagewayService } from 'src/app/services/passageway/passageway.service';

@Component({
  selector: 'app-edit-passageway',
  templateUrl: './edit-passageway.component.html',
  styleUrls: ['./edit-passageway.component.css']
})
export class EditPassagewayComponent {
    passagewayForm!: FormGroup;
    service: PassagewayService = new PassagewayService();

  constructor() { 
  }


  ngonInit(): void {
    this.passagewayForm = new FormGroup({
      floorCode: new FormControl('', [Validators.required]),
      floorCode1: new FormControl('', [Validators.required]),
    });
  }

    public async submit() {
      try {
        if (this.passagewayForm.invalid) {
          alert('Please fill all the fields');
          return;
        }
        const response = await this.service.editPassageway(
          this.floorCode?.value,
          this.floorCode1?.value,
        );
      } catch (e) {
        console.log(e);
      }
    }

    get floorCode() {
      return this.passagewayForm.get('floorCode');
    }

    get floorCode1() {
      return this.passagewayForm.get('floorCode1');
    }
    
}