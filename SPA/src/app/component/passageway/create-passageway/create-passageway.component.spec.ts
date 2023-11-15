import { ComponentFixture, TestBed } from '@angular/core/testing';

import { CreatePassagewayComponent } from './create-passageway.component';

describe('CreatePassagewayComponent', () => {
  let component: CreatePassagewayComponent;
  let fixture: ComponentFixture<CreatePassagewayComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [CreatePassagewayComponent]
    });
    fixture = TestBed.createComponent(CreatePassagewayComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
