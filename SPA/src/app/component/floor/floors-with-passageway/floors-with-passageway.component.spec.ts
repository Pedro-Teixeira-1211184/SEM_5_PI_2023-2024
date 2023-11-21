import { ComponentFixture, TestBed } from '@angular/core/testing';

import { FloorWithPassagewayComponent } from './floors-with-passageway.component';

describe('FloorWithPassagewayComponent', () => {
  let component: FloorWithPassagewayComponent;
  let fixture: ComponentFixture<FloorWithPassagewayComponent>;

  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [FloorWithPassagewayComponent]
    });
    fixture = TestBed.createComponent(FloorWithPassagewayComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
